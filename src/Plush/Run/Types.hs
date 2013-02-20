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

module Plush.Run.Types (
    Args,
    Bindings,
    ExitCode(..),   -- rexported from System.Exit

    FoundCommand(..),
    Annotation(..),

    Utility(..),
    emptyAnnotate, noArgsAnnotate,
    )
where

import qualified Data.Text as T
import System.Exit

import Plush.Types.CommandSummary


type Args = [String]
type Bindings = [(String, String)]


-- | The result of searching for a command. See "Plush.Run.BuiltIns" for
-- descriptions of special, direct, and builtin commands.
data FoundCommand = SpecialCommand
                  | DirectCommand
                  | BuiltInCommand FilePath
                  | FunctionCall
                  | ExecutableCommand FilePath
                  | UnknownCommand


data Annotation = ExpandedTo String
                | Completions [String]
                | FoundCommandAnno FoundCommand
                | CommandSummaryAnno CommandSummary
                | OptionAnno T.Text
                | UnusedAnno
    -- TODO: some of these should probably be Text rather than String


data Utility m = Utility
    { utilExecute  :: Args -> m ExitCode
    , utilAnnotate :: Args -> m [[Annotation]]
--  , utilComplete :: Args -> m Completion
    }

emptyAnnotate :: (Monad m) => Args -> m [[Annotation]]
emptyAnnotate _ = return []

noArgsAnnotate :: (Monad m) => Args -> m [[Annotation]]
noArgsAnnotate args = return $ map (const [UnusedAnno]) args
