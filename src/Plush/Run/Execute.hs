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


execute :: (PosixLike m) => CommandList -> ShellExec m ExitCode
execute cl = do
    flags <- getFlags
    if F.noexec flags
        then success
        else execCommandList cl

execCommandList :: (PosixLike m) => CommandList -> ShellExec m ExitCode
execCommandList = foldM (const execCommandItem) ExitSuccess

execCommandItem :: (PosixLike m) => (AndOrList, Execution) -> ShellExec m ExitCode
execCommandItem (ao, Sequential) = execAndOr ao
execCommandItem (_, Background) = notSupported "Background execution"

execAndOr :: (PosixLike m) => AndOrList -> ShellExec m ExitCode
execAndOr ao = foldM execAndOrItem ExitSuccess ao

execAndOrItem :: (PosixLike m) => ExitCode -> (Connector, (Sense, Pipeline))
    -> ShellExec m ExitCode
execAndOrItem ExitSuccess     (AndThen, (s, p))          = execPipeSense s p
execAndOrItem (ExitFailure e) (OrThen, (s, p))  | e /= 0 = execPipeSense s p
execAndOrItem e _ = return e

execPipeSense :: (PosixLike m) => Sense -> [Command] -> ShellExec m ExitCode
execPipeSense s p = do
    e <- sense s `fmap` execPipe p
    setLastExitCode e
    return e

sense :: Sense -> ExitCode -> ExitCode
sense Normal   e = e
sense Inverted ExitSuccess = ExitFailure 1
sense Inverted (ExitFailure e)
    | e /= 0    = ExitSuccess
    | otherwise = ExitFailure 1

execPipe :: (PosixLike m) => [Command] -> ShellExec m ExitCode
execPipe [] = exitMsg 120 "Emtpy pipeline"
execPipe [c] = execCommand c
execPipe cs = pipeline $ map execCommand cs

execCommand :: (PosixLike m) => Command -> ShellExec m ExitCode
execCommand (Simple cmd) = execSimpleCommand cmd
execCommand (Compound cmd redirects) =
    execCompoundCommand cmd redirects
execCommand (Function (Name _ name) fun) = setFun name fun >> success

execSimpleCommand :: (PosixLike m) => SimpleCommand -> ShellExec m ExitCode
execSimpleCommand (SimpleCommand [] _ (_:_)) =
    notSupported "Bare redirection"
execSimpleCommand (SimpleCommand [] as []) =
    untilFailureM setShellVar as
execSimpleCommand (SimpleCommand ws as rs) = do
    bindings <- forM as parseAssignment
    expandAndSplit ws >>= withRedirection rs . execFields bindings

execFields :: (PosixLike m) => Bindings -> [String] -> ShellExec m ExitCode
execFields bindings (cmd:args) = do
    flags <- getFlags
    when (F.xtrace flags) $ xtrace bindings cmd args
    (_, ex, _) <- commandSearch cmd
    case ex of
        UtilityAction ua  -> ua bindings args
        FunctionAction fa -> fa execFunctionBody bindings args

execFields _ [] = exitMsg 122 "Empty command"

xtrace :: (PosixLike m) => Bindings -> String -> [String] -> ShellExec m ()
xtrace bindings cmd args = do
    getVarDefault "PS4" "+ " >>= errStr -- TODO(mzero): should expand PS4
    errStrLn $ intercalate " " line
  where
    line = map showBinding bindings ++ cmd : args
    showBinding (var,val) = var ++ '=' : val

parseAssignment :: (PosixLike m) => Assignment -> ShellExec m (String, String)
parseAssignment (Assignment name w) = (name,) <$> parseAssignmentValue w

setShellVar :: (PosixLike m) => Assignment -> ShellExec m ExitCode
setShellVar (Assignment name w) = do
    v <- parseAssignmentValue w
    setVarEntry name (VarShellOnly, VarReadWrite, Just v)

parseAssignmentValue :: (PosixLike m) => Word -> ShellExec m String
parseAssignmentValue w = quoteRemoval <$> byPathParts wordExpansionActive w

execCompoundCommand :: (PosixLike m) => CompoundCommand -> [Redirect]
    -> ShellExec m ExitCode
execCompoundCommand cmd redirects = withRedirection redirects $ case cmd of
    BraceGroup cmds -> execCommandList cmds
    Subshell _cmds -> notSupported "Subshell"
    ForLoop name words_ cmds -> execFor name words_ cmds
    CaseConditional word items -> execCase word items
    IfConditional conds mElse -> execIf conds mElse
    WhileLoop test body -> execLoop True test body
    UntilLoop test body -> execLoop False test body

execFor :: (PosixLike m) => Name -> Maybe [Word] -> CommandList
    -> ShellExec m ExitCode
execFor _ Nothing _ = notSupported "missing 'in' means substitute $@"
execFor (Name _ name) (Just words_) cmds = do
    setLastExitCode ExitSuccess
    expandAndSplit words_ >>= forLoop
  where
    forLoop [] = getLastExitCode
    forLoop (w:ws) = do
        ok <- setVarEntry name (VarShellOnly, VarReadWrite, Just w)
        case ok of
            ExitSuccess -> execute cmds >> forLoop ws
            e@(ExitFailure {}) -> return e

execCase :: (PosixLike m) =>
    Word -> [([Word], Maybe CommandList)] -> ShellExec m ExitCode
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
    [(CommandList, CommandList)] -> Maybe CommandList -> ShellExec m ExitCode
execIf [] Nothing = success
execIf [] (Just e) = execute e
execIf ((c,s):css) mElse = execute c >>= \ec ->
    case ec of
        ExitFailure n | n /= 0 -> execIf css mElse
        _ -> execute s

execLoop :: (PosixLike m) =>
    Bool -> CommandList -> CommandList -> ShellExec m ExitCode
execLoop loopWhen test body = go ExitSuccess
  where
    go lastEc = execute test >>= \ec ->
        if loopWhen == isSuccess ec
            then execute body >>= go
            else return lastEc

execFunctionBody :: (PosixLike m) => FunctionBody -> ShellExec m ExitCode
execFunctionBody (FunctionBody body redirects) =
    execCompoundCommand body redirects

