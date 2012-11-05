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
    NullJson,
    runApp,
    pollApp,
    inputApp,
    historyApp,
    )
    where

import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.HashMap.Lazy as HM

import Plush.Job
import Plush.Job.History
import Plush.Job.Types
import Plush.Parser
import Plush.Server.Utilities


-- | When no request or response is needed for a JSON API, this value is passed
-- on the wire.
--
-- JSON Serailized as
--
-- @null /or/ { }@
data NullJson = NullJson
instance FromJSON NullJson where
    parseJSON (Object v) = if HM.null v then return NullJson else mzero
    parseJSON Null = return NullJson
    parseJSON _ = mzero
instance ToJSON NullJson where
    toJSON NullJson = Null


-- | Run a command in the shell.
runApp :: ShellThread
    -> JsonApplication CommandRequest (ReportOne RunResponse)
runApp shellThread cr@(CommandRequest job _ (CommandItem cmd)) = do
    r <- case parseNextCommand cmd of
        Left errs -> return $ RrParseError $ ParseErrorItem errs
        Right _ -> do
            liftIO $ submitJob shellThread cr
            return $ RrRunning RunningItem
    returnJson $ ReportOne job r



-- | Poll the state of all jobs. See 'pollJobs' for more information about
-- what status items are returned.
pollApp :: ShellThread -> JsonApplication NullJson [ReportOne StatusItem]
pollApp shellThread _req = (liftIO $ pollJobs shellThread) >>= returnJson


-- | Offer input to a running job.
inputApp :: ShellThread -> JsonApplication (ReportOne InputItem) NullJson
inputApp shellThread (ReportOne job input) = do
    liftIO $ offerInput shellThread job input
    returnJson NullJson


-- | Respond with shell history
historyApp :: ShellThread
    -> JsonApplication HistoryRequest [ReportOne HistoryItem]
historyApp shellThread HrList =
    liftIO (withHistory shellThread getAllHistory) >>= returnJson
historyApp shellThread (HrOutput jobs) =
    liftIO (withHistory shellThread $ getHistoryOutput jobs) >>= returnJson

