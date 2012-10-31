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

module Plush.Job.Types (
    JobName,

    -- * Items
    CommandItem(..),
    RunningItem(..),
    ParseErrorItem(..),
    InputItem(..),
    OutputItem(..),
    FinishedItem(..),

    -- * Aggregate Items
    RunResponse(..),
    StatusItem(..),
    HistoryItem(..),

    -- * Reports
    Report(..),
    ReportOne(..),

    -- * Other
    CommandRequest(..),
    ) where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import System.Exit
import qualified System.Posix.Signals as S


-- | Requests to the shell thread are given an identifier by the submitter.
type JobName = String



-- | A record of a command line submitted for execution.
-- JSON serialized as:
--
-- @{ cmd: /s/ }@
newtype CommandItem = CommandItem String
instance ToJSON CommandItem where
    toJSON (CommandItem s) = object [ "cmd" .= s]
instance FromJSON CommandItem where
    parseJSON (Object v) = CommandItem <$> v .: "cmd"
    parseJSON _ = mzero


-- | A notice that a job is running.
-- JSON serialized as:
--
-- @{ running: true }@
data RunningItem = RunningItem
instance ToJSON RunningItem where
    toJSON RunningItem = object [ "running" .= True ]


-- | A job can fail because it failed to parse. JSON serialized as:
--
-- @{ parseError: /s/ }@
newtype ParseErrorItem = ParseErrorItem String
instance ToJSON ParseErrorItem where
    toJSON (ParseErrorItem err) = object [ "parseError" .= err ]
instance FromJSON ParseErrorItem where
    parseJSON (Object v) = ParseErrorItem <$> v .: "parseError"
    parseJSON _ = mzero

-- | Input can be sent to a job. Input can be in the form of a string input
-- for stdin, an eof for stdin, or a signal to be sent to the process.
-- JSON serailized as one of:
--
-- @
-- { input: /s/ }
-- { eof: true }
-- { signal: /name/ }
-- @
--
-- where name is one of @\"int\"@, @\"quit\"@, or @\"kill\"@
data InputItem = InputItemInput String
               | InputItemEof
               | InputItemSignal S.Signal
instance FromJSON InputItem where
    parseJSON (Object v)
        =   on "input" (pure . InputItemInput)
        <|> on "eof" (\b -> if b then pure InputItemEof else mzero)
        <|> on "signal" (\s -> InputItemSignal <$> sigName s)
      where
        on :: FromJSON a => T.Text -> (a -> Parser i) -> Parser i
        on s f = maybe mzero (\w -> parseJSON w >>= f) $ HM.lookup s v

        sigName :: String -> Parser S.Signal
        sigName "int" = pure S.keyboardSignal
        sigName "quit" = pure S.keyboardTermination
        sigName "kill" = pure S.killProcess
        sigName _ = fail "unknown signal name"

    parseJSON _ = mzero


-- | As output is gathered, it is return in pieces, tagged by the source.
-- JSON serialized as one of:
--
-- @
-- { stdout: /s/ }
-- { stderr: /s/ }
-- { jsonout: [/v/] }
-- @
data OutputItem = OutputItemStdOut String
                | OutputItemStdErr String
                | OutputItemJsonOut [Value]
instance ToJSON OutputItem where
    toJSON (OutputItemStdOut s) = object [ "stdout" .= s]
    toJSON (OutputItemStdErr s) = object [ "stderr" .= s]
    toJSON (OutputItemJsonOut s) = object [ "jsonout" .= s]
instance FromJSON OutputItem where
    parseJSON (Object v) = (OutputItemStdOut <$> v .: "stdout")
                       <|> (OutputItemStdErr <$> v .: "stderr")
                       <|> (OutputItemJsonOut <$> v .: "jsonout")
    parseJSON _ = mzero


-- | A job can finish either because it a) failed to parse, or b) it completed
-- with an exit code (0 is considered success).
-- JSON serialized as:
--
-- @{ running: false, exitcode: /s/ }@
newtype FinishedItem = FinishedItem ExitCode
instance ToJSON FinishedItem where
    toJSON (FinishedItem ExitSuccess) = exitObject 0
    toJSON (FinishedItem (ExitFailure i)) = exitObject i
instance FromJSON FinishedItem where
    parseJSON (Object v) = do
        r <- v .: "running"
        if r
            then mzero
            else FinishedItem . asExitCode <$> v .: "exitcode"
    parseJSON _ = mzero

exitObject :: Int -> Value
exitObject i = object [ "running" .= False, "exitcode" .= i ]

asExitCode :: Int -> ExitCode
asExitCode i = if i == 0 then ExitSuccess else ExitFailure i

-- | Elements of a run response.
-- JSON serialized simply as each variant's JSON serialization
data RunResponse = RrRunning RunningItem
                 | RrParseError ParseErrorItem
instance ToJSON RunResponse where
    toJSON (RrRunning i) = toJSON i
    toJSON (RrParseError i) = toJSON i


-- | Elements of a status report. In practice, for a given job, these can only
-- occur according to this grammar:
--
-- >    SiRunning SiOutput* SiFinished
--
-- Because these are reported incrementally, and interleaved among jobs, it is
-- impractical to encode that grammar constraint in the type.
--
-- JSON serialized simply as each variant's JSON serialization
data StatusItem = SiRunning RunningItem
                | SiOutput OutputItem
                | SiFinished FinishedItem
instance ToJSON StatusItem where
    toJSON (SiRunning i) = toJSON i
    toJSON (SiOutput i) = toJSON i
    toJSON (SiFinished i) = toJSON i


-- | Elements of a history file or history report. In practice these can only
-- occur according to this grammar:
--
-- >    HiCommand ( HiParseError | HiOutput* HiFinished? )
--
-- Because these are stored and reported incrementally, it is impractical to
-- encode that grammar constraint in the type.
--
-- JSON serialized simply as each variant's JSON serialization
data HistoryItem = HiCommand CommandItem
                 | HiParseError ParseErrorItem
                 | HiFinished FinishedItem
instance ToJSON HistoryItem where
    toJSON (HiCommand i) = toJSON i
    toJSON (HiParseError i) = toJSON i
    toJSON (HiFinished i) = toJSON i
instance FromJSON HistoryItem where
    parseJSON j = HiCommand    <$> parseJSON j
              <|> HiParseError <$> parseJSON j
              <|> HiFinished   <$> parseJSON j
              <|> mzero

-- | A report of a number of items for a given job. The items are ordered, and
-- should follow the grammar for the given item type, though these reports
-- generally represent fragments of such a stream.
--
-- JSON serialized as:
--
-- @{ job: /j/, items: [/i/]}@
data Report a = Report JobName [a]
instance (ToJSON a) => ToJSON (Report a) where
    toJSON (Report j as) = object [ "job" .= j, "items" .= toJSON as ]

-- | A report of a single item for a given job.
-- JSON serialized as the serialization of the item merged with:
--
-- @{ job: /j/ }@
data ReportOne a = ReportOne JobName a
instance (ToJSON a) => ToJSON (ReportOne a) where
    toJSON (ReportOne j a) = toJSON a `extendObject` [ "job" .= j ]
instance (FromJSON a) => FromJSON (ReportOne a) where
    parseJSON j@(Object v) = ReportOne <$> v .: "job" <*> parseJSON j
    parseJSON _ = mzero

extendObject :: Value -> [Pair] -> Value
(Object o) `extendObject` ps = Object $ HM.fromList ps `HM.union` o
v `extendObject` ps = object $ ("value" .= v) : ps

-- | A request to run a job.
-- JSON serialized as
--
-- @{ job: /j/, record: /r/, cmd: /s/ }@
--
-- The @record@ parameter says if the request should be recorded in history
-- (defaults True).
data CommandRequest = CommandRequest JobName Bool CommandItem
instance FromJSON CommandRequest where
    parseJSON j@(Object v) = do
        job <- v .: "job"
        record <- v .:? "record" .!= True
        command <- parseJSON j
        return $ CommandRequest job record command
    parseJSON _ = mzero
