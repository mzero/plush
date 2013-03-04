{-
Copyright 2012-2013 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE OverloadedStrings #-}

module Plush.Run.BuiltIns.ShellState (
    complete,
    context,
    set,
    shift,
    export,
    readonly,
    unset,
    alias,
    unalias,
    env,
    )
where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Either (lefts)
import qualified Data.HashMap.Strict as M
import Data.List (foldl', sort)

import Plush.Parser
import Plush.Parser.Aliases (Aliases, aliasNameChar)
import Plush.Run.Annotate
import Plush.Run.BuiltIns.Utilities
import Plush.Run.BuiltIns.Syntax
import Plush.Run.Command
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.ShellExec
import Plush.Run.ShellFlags
import Plush.Run.Types
import Plush.Types
import Plush.Types.CommandSummary
import Plush.Utilities (readMaybe)

context :: (PosixLike m) => SpecialUtility m
context = SpecialUtility . const $ Utility contextExec noArgsAnnotate
  where
    contextExec _args = do
        ctxJson <$> getVars <*> getWorkingDirectory >>= jsonOut
        success
    ctxJson vars cwd = object
        [ "cwd" .= cwd
        , "vars" .= map varInfo (sort $ M.toList vars)
        ]
      where
        varInfo (n,(s,m,v)) = object
            [ "name" .= n
            , "scope" .= scopeStr s
            , "mode" .= modeStr m
            , "value" .= v
            ]
        scopeStr VarShellOnly = "shell" :: String
        scopeStr VarExported = "env"
        modeStr VarReadWrite = "rw" :: String
        modeStr VarReadOnly = "ro"


complete :: (PosixLike m) => SpecialUtility m
complete = SpecialUtility $ stdSyntax [argOpt 'c'] "" go
  where
    go "" [cmdline] = go' Nothing cmdline >>= jsonOut >> success
    go optC [cmdline] = case readMaybe optC of
        Just n -> go' (Just n) cmdline >>= jsonOut >> success
        _ -> exitMsg 2 "non-numeric -c argument"
    go _ _ = exitMsg 1 "One argument only"

    go' optC cmdline = do
        aliases <- getAliases
        case parseCommand aliases cmdline of
            Left errs -> return $ object [ "parseError" .= errs ]
            Right (cl, _rest) -> do
                spans <- annotate cl optC
                return $ object [ "spans" .= map jsonSpan spans ]

    jsonSpan (Span s e, annos) =
        object [ "start" .= s, "end" .= e, "annotations" .= map jsonAnno annos ]
    jsonAnno (ExpandedTo s) =
        object [ "expansion" .= s ]
    jsonAnno (FoundCommandAnno (SpecialCommand)) =
        object [ ct "special" ]
    jsonAnno (FoundCommandAnno (DirectCommand)) =
        object [ ct "direct" ]
    jsonAnno (FoundCommandAnno (BuiltInCommand fp)) =
        object [ ct "builtin", "path" .= fp ]
    jsonAnno (FoundCommandAnno FunctionCall) =
        object [ ct "function" ]
    jsonAnno (FoundCommandAnno (ExecutableCommand fp)) =
        object [ ct "executable", "path" .= fp ]
    jsonAnno (FoundCommandAnno (UnknownCommand)) =
        object [ ct "unknown" ]
    jsonAnno (CommandSummaryAnno (CommandSummary name synop _)) =
        object [ "command" .= name
               , "synopsis" .= synop
               ]
    jsonAnno (OptionAnno d) =
        object [ "option" .= d ]
    jsonAnno (Completions cs) =
        object [ "completions" .= cs ]
    jsonAnno (UnusedAnno) =
        object [ "unused" .= True ]

    ct :: String -> Pair
    ct = ("commandType" .=)


shift :: (PosixLike m) => SpecialUtility m
shift = SpecialUtility . const $ Utility shiftExec emptyAnnotate
  where
    shiftExec [] = doShift 1
    shiftExec [arg] = case readMaybe arg of
        Nothing -> exitMsg 1 "shift argument not numeric"
        Just n -> doShift n
    shiftExec _ = exitMsg 1 "shift takes at most one argument"

    doShift n = do
        args <- getArgs
        case () of
            _ | n < 0 -> exitMsg 1 "shift count can't be negative"
              | n == 0 -> success
              | n <= length args -> setArgs (drop n args) >> success
              | otherwise -> exitMsg 1 "shift count too large"


-- | The set special built-in is a marvel:
--
--   * It can output, but not set, shell variables: @set@
--
--   * It can set, but not output, shell positional parameters: @set a b c@
--
--   * It can set, in two different ways, shell flags: @set -x@ and @set -o xtrace@
--
--   * It can output, in two different ways, shell flags: @set -o@ and @set +o@
set :: (PosixLike m) => SpecialUtility m
set = SpecialUtility . const $ Utility setExec setAnno
  where
    setExec args = case args of
        [] -> showVars >> success
        ["-o"] -> showFlags reportFmt >> success
        ["+o"] -> showFlags scriptFmt >> success
        _ -> do
            let (flagF, args') = processFlagArgs args
            getFlags >>= setFlags . flagF
            case args' of
                ("--":args'') -> setArgs args''
                [] -> return ()
                _ -> setArgs args'
                -- TODO: should error if there are any - or + args left
            success

    setAnno = emptyAnnotate -- TODO: should really annotate these flags

    showVars = getVars >>= mapM_ (outStrLn . varFmt) . sort . M.toList
    varFmt (n,(_,_,v)) = n ++ "=" ++ maybe "" quote v

    showFlags fmt = do
        flags <- getFlags
        mapM_ (outStrLn . fmt flags) flagDescriptions

    reportFmt flags desc =
        padTo 17 (fdLongName desc) ++ onOff (fdGetter desc flags)
    padTo n s = s ++ replicate (n - length s) ' '
    onOff b = if b then "on" else "off"

    scriptFmt flags desc =
        "set" ++ plusMinus (fdGetter desc flags) ++ (fdLongName desc)
    plusMinus b = if b then " -o " else " +o "

unset :: (PosixLike m) => SpecialUtility m
unset = SpecialUtility $ stdSyntax options "" go
  where
    options = [ flag 'v', flag 'f' ]

    go "v" names = unsetVars names >>= returnError
    go "f" names = unsetFuns names
    go _flags names = unsetVars names >>= ifError returnError (unsetFuns names)

    unsetVars names = untilErrorM $ map unsetVarEntry names
    unsetFuns names = mapM_ unsetFun names >> success

modifyVar cmdName hasModifier mkVarEntry = SpecialUtility $ stdSyntax options "" go
  where
    options = [ flag 'p' ]  -- echo exports

    go "p" [] = showVars >> success
    go _flags nameVals = untilErrorM (map defVar nameVals) >>= returnError

    showVars = getVars >>= mapM_ (outStr . varFmt) . sort . M.toList
    varFmt (n, ve@(_, _, val)) | hasModifier ve =
        case val of
            Just v -> cmdName ++ " " ++ n ++ "=" ++ quote v ++ "\n"
            Nothing -> cmdName ++ " " ++ n ++ "\n"
    varFmt _ = ""

    defVar nameVal = do
        case break (== '=') nameVal of
            ([], v) -> errStrLn ("missing variable name: " ++ v) >> failure
            (name, ('=':v)) -> setVarEntry name $ mkVarEntry (Just v)
            (name, _) -> setVarEntry name $ mkVarEntry Nothing

export :: (PosixLike m) => SpecialUtility m
export = modifyVar "export" isExported (\v -> (VarExported, VarReadWrite, v))
  where
    isExported (VarExported, _, _) = True
    isExported _ = False

readonly :: (PosixLike m) => SpecialUtility m
readonly = modifyVar "readonly" isReadOnly (\v -> (VarShellOnly, VarReadOnly, v))
  where
    isReadOnly (_, VarReadOnly, _) = True
    isReadOnly _ = False

alias :: (PosixLike m) => DirectUtility m
alias = DirectUtility $ stdSyntax options "" (doAlias . format)
  where
    options = [ flag 'p' ]
        -- not POSIX, but common and matches -p in other
        -- POSIX commands like readonly and export

    doAlias fmt [] = do
        getAliases >>= mapM_ (outStrLn . uncurry fmt) . sort . M.toList
        success
    doAlias fmt args =
        modifyAliases $ \m -> foldl' (doOne fmt) ([],m) args

    doOne fmt (us, m) arg = let (n,w) = break (== '=') arg in
        if all aliasNameChar n
            then case w of
                [] -> ((maybe (Left $ "unknown alias: " ++ n)
                              (\v -> Right $ fmt n v)
                              $ M.lookup n m):us, m)
                (_:v) -> (us, M.insert n v m)  -- the first char is the '='
            else (Left ("invalid alias name: " ++ n) : us, m)

    format flags n v = prefix flags ++ n ++ '=' : quote v
    prefix flags = if ('p' `elem` flags) then "alias " else ""

unalias :: (PosixLike m) => DirectUtility m
unalias = DirectUtility $ stdSyntax [ flag 'a' ] "" doUnalias
  where
    doUnalias "a" _ = setAliases M.empty >> success
    doUnalias _ args = modifyAliases $ \m -> foldl' remove ([], m) args
    remove (us, m) n = if M.member n m
        then (us, M.delete n m)
        else ((Left $ "unknown alias: " ++ n):us, m)


modifyAliases :: (PosixLike m) =>
    (Aliases -> ([Either String String], Aliases)) -> ShellExec m ExitCode
modifyAliases f = do
    (msgs, newAliases) <- f `fmap` getAliases
    setAliases newAliases
    mapM_ (either errStrLn outStrLn) $ reverse msgs
    if null $ lefts msgs then success else failure

quote :: String -> String
quote v = '\'' : concatMap qchar v ++ "'"
  where
    qchar '\'' = "'\"'\"'"
    qchar c = [c]

env :: (PosixLike m) => DirectUtility m
env = DirectUtility $ stdSyntax [] "" doEnv
  where
    doEnv :: (PosixLike m) => String -> Args -> ShellExec m ExitCode
    doEnv _flags (_:_) = exitMsg 1 "only env with no arguments is currently supported"
    doEnv _ _ = do
        bindings <- getEnv
        mapM_ (\(k, v) -> outStrLn $ k ++ "=" ++ v) bindings
        success
