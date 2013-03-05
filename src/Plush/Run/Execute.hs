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

{-# Language TupleSections #-}

module Plush.Run.Execute (
    execute,
    )
where

import Control.Monad
import Data.Functor
import Data.List (intercalate)

import Plush.Run.Command
import Plush.Run.Expansion
import Plush.Run.Pattern
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.Redirection
import Plush.Run.ShellExec
import qualified Plush.Run.ShellFlags as F
import Plush.Run.Types
import Plush.Types


-- | Base case for compound operations (folds, etc.)
-- > defaultSuccess = StStatus ExitSuccess
defaultSuccess :: ShellStatus
defaultSuccess = StStatus ExitSuccess


execute :: (PosixLike m) => CommandList -> ShellExec m ShellStatus
execute cl = do
    flags <- getFlags
    if F.noexec flags
        then success
        else execCommandList cl

execCommandList :: (PosixLike m) => CommandList -> ShellExec m ShellStatus
execCommandList = shellSequence . map listItem
  where
    listItem (ao, Sequential) = execAndOr ao
    listItem (_, Background)  = notSupported "Background execution"

execAndOr :: (PosixLike m) => AndOrList -> ShellExec m ShellStatus
execAndOr = foldM andOrItem defaultSuccess
  where
    andOrItem (StStatus ExitSuccess)     (AndThen, (s, p)) = execPipeSense s p
    andOrItem (StStatus (ExitFailure _)) (OrThen, (s, p))  = execPipeSense s p
    andOrItem e _ = return e

execPipeSense :: (PosixLike m) => Sense -> [Command] -> ShellExec m ShellStatus
execPipeSense s p = do
    st <- execPipe p
    case st of
        StStatus e -> let e' = sense s e
                      in setLastExitCode e' >> return (StStatus e')
        _ -> return st
  where
    sense Normal   e = e
    sense Inverted ExitSuccess     = ExitFailure 1
    sense Inverted (ExitFailure _) = ExitSuccess

execPipe :: (PosixLike m) => [Command] -> ShellExec m ShellStatus
execPipe [] = shellError 120 "Emtpy pipeline"
execPipe [c] = execCommand c
execPipe cs = StStatus <$> pipeline [statusExitCode <$> execCommand c | c <- cs]

execCommand :: (PosixLike m) => Command -> ShellExec m ShellStatus
execCommand (Simple cmd)                 = execSimpleCommand cmd
execCommand (Compound cmd redirects)     = execCompoundCommand cmd redirects
execCommand (Function (Name _ name) fun) = setFun name fun >> success

execSimpleCommand :: (PosixLike m) => SimpleCommand -> ShellExec m ShellStatus
execSimpleCommand (SimpleCommand ws as rs) = do
    bindings <- forM as parseAssignment
    cmdAndArgs <- expandAndSplit ws

    flags <- getFlags
    when (F.xtrace flags) $ xtrace bindings cmdAndArgs

    withRedirection rs $ do
        case cmdAndArgs of
            [] -> untilFailureM (uncurry setShellVar) bindings
            (cmd:args) -> do
                (_, ex, _) <- commandSearch cmd
                case ex of
                    UtilityAction ua  -> ua bindings args
                    FunctionAction fa -> fa execFunctionBody bindings args
  where
    parseAssignment (Assignment name w) = do
        we <- byPathParts wordExpansionActive w
        return (name, quoteRemoval we)

xtrace :: (PosixLike m) => Bindings -> [String] -> ShellExec m ()
xtrace bindings cmdAndArgs = do
    getVarDefault "PS4" "+ " >>= errStr -- TODO(mzero): should expand PS4
    errStrLn $ intercalate " " line
  where
    line = map showBinding bindings ++ cmdAndArgs
    showBinding (var,val) = var ++ '=' : val

setShellVar :: (PosixLike m) => String -> String -> ShellExec m ShellStatus
setShellVar name v = setVarEntry name (VarShellOnly, VarReadWrite, Just v)


execCompoundCommand :: (PosixLike m) => CompoundCommand -> [Redirect]
    -> ShellExec m ShellStatus
execCompoundCommand cmd redirects = withRedirection redirects $ case cmd of
    BraceGroup cmds -> execCommandList cmds
    Subshell cmds -> subshell $ execCommandList cmds
    ForLoop name words_ cmds -> execFor name words_ cmds
    CaseConditional word items -> execCase word items
    IfConditional conds mElse -> execIf conds mElse
    WhileLoop test body -> execLoop True test body
    UntilLoop test body -> execLoop False test body

execFor :: (PosixLike m) => Name -> Maybe [Word] -> CommandList
    -> ShellExec m ShellStatus
execFor (Name _ name) inWords cmds =
    maybe getArgs expandAndSplit inWords >>= forLoop
  where
    forLoop = runLoop True . map (\w -> (setShellVar name w, cmds))

execCase :: (PosixLike m) =>
    Word -> [([Word], Maybe CommandList)] -> ShellExec m ShellStatus
execCase word items = wordExpansionActive word >>= check items . quoteRemoval
  where
    check [] _ = success
    check ((ps, mcl):is) w = match ps w >>= \m ->
        if m
            then maybe success execute mcl
            else check is w
    match [] _ = return False
    match (p:ps) w = wordExpansionActive p >>= \q ->
        if patternMatch (makePattern q) w
            then return True
            else match ps w

execIf :: (PosixLike m) =>
    [(CommandList, CommandList)] -> Maybe CommandList -> ShellExec m ShellStatus
execIf [] Nothing = success
execIf [] (Just e) = execute e
execIf ((c,s):css) mElse = execute c >>= \st ->
    case st of
        StStatus ExitSuccess     -> execute s
        StStatus (ExitFailure _) -> execIf css mElse
        _                        -> return st

execLoop :: (PosixLike m) =>
    Bool -> CommandList -> CommandList -> ShellExec m ShellStatus
execLoop loopWhen test body = runLoop loopWhen $ repeat (execute test, body)


runLoop :: (PosixLike m) =>
    Bool -> [(ShellExec m ShellStatus, CommandList)] -> ShellExec m ShellStatus
runLoop loopWhen = flip go defaultSuccess
  where
    go _ (StBreak n)        | n == 1 = return defaultSuccess
                            | n > 1  = return (StBreak $ n - 1)
                            | otherwise = shellError 120 "Bad break"

    go as (StContinue n)    | n == 1 = go as defaultSuccess
                            | n > 1  = return (StContinue $ n - 1)
                            | otherwise = shellError 120 "Bad continue"

    go ((pre,body):as) st@(StStatus _) = do
        p <- pre
        case p of
            StStatus ec | loopWhen == isSuccess ec -> execute body >>= go as
            _ -> return st

    go _ st = return st



execFunctionBody :: (PosixLike m) => FunctionBody -> ShellExec m ShellStatus
execFunctionBody (FunctionBody body redirects) =
    handleReturn <$> execCompoundCommand body redirects
  where
    handleReturn (StReturn ec) = StStatus ec
    handleReturn st = st
