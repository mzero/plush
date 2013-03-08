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

module Plush.Run.ExecuteType (
    ExecuteType(..),
    execType,
    )
where

import Control.Applicative ((<*>))
import Data.Functor
import Data.Monoid

import Plush.Run.Command
import Plush.Run.Expansion
import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Types


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
        ForLoop _ _ cmdlist -> typeCommandList cmdlist
        CaseConditional _ items -> minimum <$>
            mapM (maybe (return mempty) typeCommandList . snd) items
        IfConditional conds mElse -> minimum <$>
            mapM typeCommandList
                (maybe id (:) mElse $ concatMap (\(c,d)->[c,d]) conds)
        WhileLoop condition body ->
            min <$> typeCommandList condition <*> typeCommandList body
        UntilLoop condition body ->
            min <$> typeCommandList condition <*> typeCommandList body
    typeCommand (Function {}) = return ExecuteForeground

    typeSimple (SimpleCommand [] _ _) = return ExecuteForeground
    typeSimple (SimpleCommand ws _ _) =
        expandPassive ws >>= typeFields . map quoteRemoval

    typeFields [] = return ExecuteForeground
    typeFields (cmd:_) = do
        (fc, _, _) <- commandSearch cmd
        return $ typeFound fc

    typeFound SpecialCommand = ExecuteForeground
    typeFound DirectCommand = ExecuteForeground
    typeFound (BuiltInCommand _) = ExecuteMidground
    typeFound FunctionCall = ExecuteForeground
        -- TODO: analyze function bodies to see if they can be midgrounded
    typeFound (ExecutableCommand _) = ExecuteMidground
    typeFound UnknownCommand = ExecuteForeground
