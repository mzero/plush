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
    -- * Common
    Args,
    Bindings,

    -- * Return Values
    -- $exit
    ExitCode(..),
    intToExitCode, exitCodeToInt,

    ErrorCode(..),

    ShellStatus(..),
    statusExitCode,

    -- * Commands
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

{- $exit
POSIX has a concept of "Exit Status", which is a numeric value that processes
exits with at completion. It is limited to the range 0..255 by the spec.
The shell and other utilities generally treat 0 as "success" and other values
as "failure".

Haskell's "System.Exit" defines the type 'ExitCode'. However, this type is
slightly more general, in that it separates the notion of success and failure
from the numeric code (which is only supplied on failure). This leads to the
unfortunate possibility of the value @(ExitFailure 0)@. Is this success or
failure in a POSIX context?

In the interest of harmony with Haskell's common usage, plush uses 'ExitCode'
to reprsent "Exit Status". Further, to make code clear and consistent, it
only distinguishes success from failure based on the constructor. The type is
reexported in this module because most of the plush code shouldn't be using
the standard System libraries.
-}

-- | Convert a numeric exit status to an 'ExitCode'. Handles selection of the
-- correct constructor. It is acceptable, and preferable, to apply 'ExitFailure'
-- to non-zero constant. Otherwise, use this function.
intToExitCode :: Int -> ExitCode
intToExitCode i = if i == 0 then ExitSuccess else ExitFailure i

-- | Extract the numeric exit status from an 'ExitCode'
exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n


-- | Shell errors exit the shell with an exit status. This type wraps the
-- normal 'ExitCode' for such a purpose. It provides some type clarity for
-- those situations where a shell error is possible, but other status values
-- are not.
newtype ErrorCode = ErrorCode { errorExitCode :: ExitCode }
    deriving (Eq)


-- | The result of special utilities, and of execution within the shell itself,
-- can be more complicated than an 'ExitCode'. Importantly, it includes the
-- idea of "shell error" that causes shell termination in non-interactive
-- shells, and abort to the user intput loop in interactive ones. It also
-- includes the flow control operations like @break@ and @continue@.
data ShellStatus = StStatus ExitCode    -- ^ a normal "exit status"
                 | StError ErrorCode    -- ^ a "shell error"
                 | StExit ExitCode      -- ^ exit utility invoked
                 | StReturn ExitCode    -- ^ return from function or script
                 | StBreak Int          -- ^ break looping command
                 | StContinue Int       -- ^ continue looping command
    deriving (Eq)

-- | The exit status corresponding to a shell status.
-- Unhandled control status values are mapped to error exit status values.
statusExitCode :: ShellStatus -> ExitCode
statusExitCode (StStatus ec) = ec
statusExitCode (StError ec) = errorExitCode ec
statusExitCode (StExit ec) = ec
statusExitCode (StReturn ec) = ec
statusExitCode _ = ExitFailure 123


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



data Utility m e = Utility
    { utilExecute  :: Args -> m e
    , utilAnnotate :: Args -> m [[Annotation]]
--  , utilComplete :: Args -> m Completion
    }


emptyAnnotate :: (Monad m) => Args -> m [[Annotation]]
emptyAnnotate _ = return []

noArgsAnnotate :: (Monad m) => Args -> m [[Annotation]]
noArgsAnnotate args = return $ map (const [UnusedAnno]) args
