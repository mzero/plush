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

module Plush.Run.BuiltIns.Utilities (

    -- $types
    SpecialUtility(..), DirectUtility(..), BuiltInUtility(..),
    unSpecial, unDirect, unBuiltIn,

    )
    where

import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Run.Types
import Plush.Types.CommandSummary

-- $types
-- Utilities built into the shell can be treated in several different ways.
-- The kind of utility determines the environment in which it is executed, and
-- how it may, or may not, affect shell state. The actual action for all
-- built in utilities is @'ShellUtility' m@. These types wrap that and form
-- a promise for how the utility operates.


-- | Special Built-In Utilities (§2.14)
-- These found without PATH search, and execute directly within the shell.
-- In particular, variable assignments made with these commands affect the
-- shell.
newtype SpecialUtility m = SpecialUtility (CommandSummary -> ShellUtility m)

-- | Direct Built-In Utilities (§2.9.1)
-- These are executed without PATH search. These commands may have side-effects
-- that affect the shell environment (for example cd changes the shells cwd),
-- but are invoked as if they were external commands. In particular, variable
-- assignments only affect the environment variables available to these commands
-- and not the shell's variables.
newtype DirectUtility m = DirectUtility (CommandSummary -> ShellUtility m)

-- | Regular Built-In Utilities (§2.9.1)
-- If PATH search succeeds in finding an executable for these, then the built-in
-- version may be executed instead. However, it is run in an environment
-- equivalent to one the executable would have run in. These built-ins can't
-- affect the shell state.
newtype BuiltInUtility m = BuiltInUtility (CommandSummary -> Utility m)



unSpecial :: (PosixLike m) => String -> SpecialUtility m -> ShellUtility m
unSpecial name (SpecialUtility csu) = bindSummary name csu

unDirect :: (PosixLike m) => String -> DirectUtility m -> ShellUtility m
unDirect name (DirectUtility csu) = bindSummary name csu

unBuiltIn :: (PosixLike m) => String -> BuiltInUtility m -> ShellUtility m
unBuiltIn name (BuiltInUtility csu) = bindSummary name csu'
  where
    csu' summ = case csu summ of
        (Utility e a) -> Utility (runLifted e) (runLifted a)
    runLifted exec args = lift $ exec args


bindSummary :: (PosixLike m) =>
    String -> (CommandSummary -> ShellUtility m) -> ShellUtility m
bindSummary name csu = Utility (bind utilExecute) (bind utilAnnotate)
  where
    bind f args = getSummary name >>= ($args) . f . csu . fromMaybe s0
    s0 = CommandSummary (T.pack name) T.empty []

