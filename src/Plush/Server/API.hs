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

module Plush.Server.API (
    runApp,
    pollApp,
    inputApp,
    )
    where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.HashMap.Lazy as HM

import Plush.Job
import Plush.Parser
import Plush.Server.Utilities

data PlushRequest = RunCommand String String
instance FromJSON PlushRequest where
    parseJSON (Object v) = RunCommand <$> v .: "job" <*> v .: "cmd"
    parseJSON _ = mzero

data PlushResponse = ParseError String String
                  | JobRunning String
instance ToJSON PlushResponse where
    toJSON (ParseError j err) = object [ "job" .= j, "parseError" .= err ]
    toJSON (JobRunning j) = object [ "job" .= j, "running" .= True ]

-- | Run a command in the shell.
--
-- [Request]
-- @/j/@ is a string to identify the job. @/c/@ is the command to execute.
--
-- @{ job: /j/, cmd: /c/ }@
--
-- [Response]
-- One of:
--
-- @{ job: /j/, parseError: /err/ }@
--
-- @{ job: /j/, running: true }@
--
runApp :: ShellThread -> JsonApplication PlushRequest PlushResponse
runApp shellThread (RunCommand job cmd) =
    case parseNextCommand cmd of
        Left errs -> returnJson $ ParseError job errs
        Right (cl, _rest) -> do
            liftIO $ submitJob shellThread job cl
            returnJson $ JobRunning job

data NullRequest = NullRequest
instance FromJSON NullRequest where
    parseJSON (Object v) = if HM.null v then return NullRequest else mzero
    parseJSON Null = return NullRequest
    parseJSON _ = mzero

data PlushStatus = ReportStdOut String String
                | ReportStdErr String String
                | ReportJsonOut String [Value]
                | ReportRunning String
                | ReportDone String Int
instance ToJSON PlushStatus where
    toJSON (ReportStdOut j s) = object [ "job" .= j, "stdout" .= s]
    toJSON (ReportStdErr j s) = object [ "job" .= j, "stderr" .= s]
    toJSON (ReportJsonOut j v) = object [ "job" .= j, "jsonout" .= v]
    toJSON (ReportRunning j) = object [ "job" .= j, "running" .= True]
    toJSON (ReportDone j e) =
        object [ "job" .= j, "running" .= False, "exitcode" .= e ]

-- | Poll the state of all jobs.
--
-- [Request]
--
-- @null /or/ { }@
--
-- [Response]
-- Replies with a JSON array of status entries. A given job may have multiple
-- entries in this array. Each entry can be one of the following:
--
-- @{ job: /j/, stdout: /s/ }@
--
-- @{ job: /j/, stderr: /s/ }@
--
-- @{ job: /j/, running: /bool/ }@
pollApp :: ShellThread -> JsonApplication NullRequest [PlushStatus]
pollApp shellThread _req = do
    rs <- liftIO $ reviewJobs shellThread report
    returnJson $ concat rs
  where
    report job (Running rs) = do
        oi' <- gatherOutput rs
        return $ reportOis job oi' ++ [ReportRunning job]
    report job (JobDone e oi') =
        return $ reportOis job oi' ++ [ReportDone job e]

    reportOis job = map (reportOi job)
    reportOi job (OiStdOut s) = ReportStdOut job s
    reportOi job (OiStdErr s) = ReportStdErr job s
    reportOi job (OiJsonOut vs) = ReportJsonOut job vs



data OfferInput = OfferInput JobName String Bool
instance FromJSON OfferInput where
    parseJSON (Object v) = OfferInput
        <$> v .: "job"
        <*> v .:? "input" .!= ""
        <*> v .:? "eof" .!= True
    parseJSON _ = mzero


data NullResponse = NullResponse
instance ToJSON NullResponse where
    toJSON NullResponse = toJSON ()

-- | Offer input to a running job.
-- @/j/@ is a string to identify the job. @/s/@ is the input. If the @eof@
-- flag is set, it is sent after the input (which can be the empty string).
--
-- @{ job: /j/, input: /s/, eof: /bool/}@
--
-- [Response]
-- Replies with null
--
-- @null /or/ { }@
inputApp :: ShellThread -> JsonApplication OfferInput NullResponse
inputApp shellThread (OfferInput job input eof) = do
    liftIO $ offerInput shellThread job input eof
    returnJson NullResponse
