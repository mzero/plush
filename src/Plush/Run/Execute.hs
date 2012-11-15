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

{-# Language TupleSections #-}

module Plush.Run.Execute (
    shellExec,

    ExecuteType(..),
    execType,
    )
where

import Control.Monad
import Control.Applicative ((<*>))
import Data.Functor
import Data.Monoid

import Plush.Run.Command
import Plush.Run.Expansion
import Plush.Run.Posix
import Plush.Run.Redirection
import Plush.Run.ShellExec
import Plush.Run.Types
import Plush.Types


shellExec :: (PosixLike m) => CommandList -> ShellExec m ()
shellExec cl = execCommandList cl >> return ()

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
execCommand (Function {}) = notSupported "Execute function"

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
    (_, ex, _) <- commandSearch cmd
    ex bindings args
execFields _ [] = exitMsg 122 "Empty command"

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
    BraceGroup _cmds -> notSupported "BraceGroup"
    Subshell _cmds -> notSupported "Subshell"
    ForClause name words_ cmds -> execFor name words_ cmds
    IfClause _condition _consequent _alts -> notSupported "IfClause"
    WhileClause _condition _cmds -> notSupported "WhileClause"
    UntilClause _condition _cmds -> notSupported "UntilClause"

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
            ExitSuccess -> shellExec cmds >> forLoop ws
            e@(ExitFailure {}) -> return e

data ExecuteType = ExecuteForeground | ExecuteMidground | ExecuteBackground
  deriving (Eq, Ord, Bounded)

instance Monoid ExecuteType where
  mempty = ExecuteForeground
  mappend = min
  mconcat [] = mempty
  mconcat es = minimum es


execType :: (PosixLike m) => CommandList -> ShellExec m ExecuteType
execType = typeCommandList
  where
    typeCommandList cl = mconcat <$> mapM typeCommandItem cl

    typeCommandItem (ao, Sequential) = typeAndOr ao
    typeCommandItem (_, Background) = return ExecuteBackground

    typeAndOr ao = mconcat <$> mapM (\(_, (_, p)) -> typePipe p) ao

    typePipe p = mconcat <$> mapM typeCommand p

    typeCommand (Simple cmd) = typeSimple cmd
    typeCommand (Compound cmd _) = case cmd of
        BraceGroup cmdlist -> typeCommandList cmdlist
        -- Things in the subshell can't change the state of this shell.
        Subshell {} -> return ExecuteMidground
        ForClause _ _ cmdlist -> typeCommandList cmdlist
        IfClause condition consequent alternatives -> minimum <$>
            mapM typeCommandList (condition : consequent : alternatives)
        WhileClause condition body ->
            min <$> typeCommandList condition <*> typeCommandList body
        UntilClause condition body ->
            min <$> typeCommandList condition <*> typeCommandList body
    typeCommand (Function {}) = return ExecuteForeground

    typeSimple (SimpleCommand [] _ _) = return ExecuteForeground
    typeSimple (SimpleCommand ws _ _) =
        expandAndSplit ws >>= typeFields

    typeFields [] = return ExecuteForeground
    typeFields (cmd:_) = do
        (fc, _, _) <- commandSearch cmd
        return $ typeFound fc

    typeFound SpecialCommand = ExecuteForeground
    typeFound DirectCommand = ExecuteForeground
    typeFound (BuiltInCommand _) = ExecuteMidground
    typeFound (ExecutableCommand _) = ExecuteMidground
    typeFound UnknownCommand = ExecuteForeground
