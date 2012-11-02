{-
Copyright 2012 Google Inc. All Rights Reserved.

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
    export,
    readonly,
    unset,
    env,
    )
where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as M
import Data.List (sort)
import Data.Maybe (listToMaybe)

import Plush.Parser
import Plush.Run.Annotate
import Plush.Run.BuiltIns.Utilities
import Plush.Run.BuiltIns.Syntax
import Plush.Run.Command
import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Run.ShellFlags
import Plush.Run.Types
import Plush.Types
import Plush.Types.CommandSummary

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
    go optC [cmdline] = case maybeRead optC of
        Just n -> go' (Just n) cmdline >>= jsonOut >> success
        _ -> exitMsg 2 "non-numeric -c argument"
    go _ _ = exitMsg 1 "One argument only"

    go' optC cmdline =
        case parseNextCommand cmdline of
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

maybeRead :: (Read a) => String -> Maybe a
maybeRead = listToMaybe . map fst . filter (null . snd) . reads


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

    go "v" names = untilFailureM unsetVarEntry names
    go "f" names = untilFailureM unsetFunEntry names
    go _flags names = untilFailureM unsetVarEntry names
                      -- TODO: enable this eventually
                      -- `andThenM` untilFailureM unsetFunEntry names

    unsetFunEntry _name = notSupported "unset -f"

modifyVar cmdName hasModifier mkVarEntry = SpecialUtility $ stdSyntax options "" go
  where
    options = [ flag 'p' ]  -- echo exports

    go "p" [] = showVars >> success
    go _flags nameVals = untilFailureM defVar nameVals

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
