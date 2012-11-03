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
shellExec cl =  execCommandList cl
  where
    execCommandList = mapM_ execCommandItem

    execCommandItem (ao, Sequential) = execAndOr ao
    execCommandItem (_, Background) = notSupported "Background execution"

    execAndOr ao = foldM execAndOrItem ExitSuccess ao

    execAndOrItem ExitSuccess     (AndThen, (s, p))          = execPipeSense s p
    execAndOrItem (ExitFailure e) (OrThen, (s, p))  | e /= 0 = execPipeSense s p
    execAndOrItem e _ = return e

    execPipeSense s p = do
        e <- sense s `fmap` execPipe p
        setLastExitCode e
        return e

    sense Normal   e = e
    sense Inverted ExitSuccess = ExitFailure 1
    sense Inverted (ExitFailure e)
        | e /= 0    = ExitSuccess
        | otherwise = ExitFailure 1

    execPipe [] = exitMsg 120 "Emtpy pipeline"
    execPipe [c] = execCommand c
    execPipe cs = pipeline $ map execCommand cs

    execCommand (Command [] _ (_:_)) = notSupported "Bare redirection"
    execCommand (Command [] as []) = untilFailureM setShellVars as
    execCommand (Command ws as rs) = do
        bindings <- forM as parseAssignment
        expandAndSplit ws >>= withRedirection rs . execFields bindings

    execFields bindings (cmd:args) = do
        (_, ex, _) <- commandSearch cmd
        ex bindings args
    execFields _ [] = exitMsg 122 "Empty command"

    parseAssignment (Assignment name w) = (name,) <$> parseAssignmentValue w

    parseAssignmentValue w = quoteRemoval <$> byPathParts wordExpansionActive w

    setShellVars :: (PosixLike m) => Assignment -> ShellExec m ExitCode
    setShellVars (Assignment name w) = do
        v <- parseAssignmentValue w
        setVarEntry name (VarShellOnly, VarReadWrite, Just v)

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

    typeCommand (Command [] _ _) = return ExecuteForeground
    typeCommand (Command ws _ _) =
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
