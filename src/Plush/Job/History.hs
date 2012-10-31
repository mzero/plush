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

module Plush.Job.History (
    History,
    HistoryFile,

    initHistory,
    startHistory,
    writeHistory,
    writeOutput,
    endHistory,
    getAllHistory,

    ) where


import qualified Control.Exception as Exception
import Control.Monad (when)
import Data.Aeson (encode, fromJSON, Result(..), ToJSON)
import Data.List (isSuffixOf)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Traversable as TR
import System.FilePath ((</>), takeDirectory)
import System.Locale
import System.Posix

import Plush.Job.Output
import Plush.Job.Types
import Plush.Run
import Plush.Run.Posix (getDirectoryContents, write)
import Plush.Run.ShellExec (getVar)

data History = History { histDir :: Maybe FilePath }
data HistoryFile = HistoryFile { hfCmd :: Maybe Fd, hfOut :: Maybe Fd }

initHistory :: Runner -> IO History
initHistory runner = do
    (home, _) <- run (getVar "HOME") runner
    hdir <- TR.mapM mkHistDir home `catchAllAs` Nothing
    return $ History hdir
        -- TODO: should return latest time stamp in directory
  where
    mkHistDir h = do
        let hdir = h </> ".plush/history"
        mkPath hdir
        return hdir

    -- TODO: refactor these with the ones from FileSystem.hs
    mkNorm fp = createDirectory fp accessModes
    mkPath fp = do
        s <- (isDirectory `fmap` getFileStatus fp) `catchAllAs` False
        when (not s) $ do
            mkPath (takeDirectory fp)
            mkNorm fp


startHistory :: CommandRequest -> History -> IO (History, HistoryFile)
startHistory (CommandRequest _ True (CommandItem _cmd))
             h@(History (Just hdir)) = do
    prefix <- prefixString
    next <- findFree prefix ".cmd" (0 :: Int)
    cmdFd <- openFd (next ++ ".cmd") ReadWrite (Just ownerRWMode) defaultFileFlags
    outFd <- openFd (next ++ ".out") ReadWrite (Just ownerRWMode) defaultFileFlags
    return (h, HistoryFile (Just cmdFd) (Just outFd))
  where
    findFree prefix suffix n = do
        let name = hdir </> prefix ++ show n
        let probe = name ++ suffix
        exists <- (getFileStatus probe >> return True) `catchAllAs` False
            -- TODO: should check prefixes
        if exists
            then findFree prefix suffix (n + 1)
            else return name -- TODO: add on command text

    ownerRWMode = ownerReadMode `unionFileModes` ownerWriteMode
startHistory _ h = return (h, HistoryFile Nothing Nothing)

prefixString :: IO String
prefixString = do
    t <- getCurrentTime
    return $ formatTime defaultTimeLocale "%Y%m%d.%H%M" t ++ "."

writeHistory :: HistoryFile -> HistoryItem -> IO ()
writeHistory = writeJson . hfCmd

writeOutput :: HistoryFile -> OutputItem -> IO ()
writeOutput = writeJson . hfOut

writeJson :: (ToJSON a) => Maybe Fd -> a -> IO ()
writeJson mfd s = maybe (return ()) writeJson' mfd
  where
    writeJson' fd = write fd (encode s) >> write fd nl
    nl = LT.encodeUtf8 $ LT.pack "\n"

endHistory :: HistoryFile -> IO ()
endHistory hf = close (hfCmd hf) >> close (hfOut hf)
  where
    close (Just fd) = closeFd fd `catchAllAs` ()
    close Nothing = return ()


-- | Return all of history. For now... uhm, yeah, simplest thing that could
-- possibly work.
getAllHistory :: History -> IO [ReportOne HistoryItem]
getAllHistory h = case (histDir h) of
    Nothing -> return []
    Just hdir -> do
        histFiles <- filter cmdFileName `fmap` getDirectoryContents hdir
        concat `fmap` mapM (readHistoryFile hdir) histFiles
  where
    cmdFileName = (".cmd" `isSuffixOf`)

    readHistoryFile hdir f = (`catchAllAs` []) $ do
        -- TODO: handle errors here gracefully
        fd <- openFd (hdir </> f) ReadOnly Nothing defaultFileFlags
        jsons <- outputStreamJson fd >>= getAvailable
        closeFd fd
        return $ map (ReportOne f) . successes . map fromJSON $ jsons

    successes [] = []
    successes (Success a : ps) = a : successes ps
    successes (Error _   : ps) =     successes ps


catchAllAs :: IO a -> a -> IO a
catchAllAs act def = act `Exception.catch` ignore def
  where
    -- TODO: catch something more specific
    ignore :: a -> Exception.SomeException -> IO a
    ignore = const . return

