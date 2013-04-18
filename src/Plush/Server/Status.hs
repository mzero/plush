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

{-# LANGUAGE OverloadedStrings #-}

module Plush.Server.Status (
    ServerType(..),
    ServerInfo(..),
    readServerInfo,
    writeServerInfo,
    removeServerInfo,

    serverTypePrefix,
    serverInfoPrefix,

    allKnownServers,

    makeLogger,
    startUrl,

    sshControlFilePath,
    )
    where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (mzero)
import qualified Control.Monad.Exception as Ex
import Data.Aeson
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Time (formatTime, getZonedTime)
import System.FilePath ((</>))
import System.Posix (getEnv, ownerModes, ownerReadMode, ownerWriteMode,
    unionFileModes)

import Plush.Run.Posix
import Plush.Run.Posix.IO ()
import Plush.Run.Posix.Utilities
    (createPath, readAllFile, writeAllFile, writeStrLn)


data ServerType = LocalServer | RemoteServer String

-- | Information about a server.
--
-- If for a remote server, the pid and port are the local pid (of the ssh
-- master) and port (of the locally forwarded connection.)
data ServerInfo = ServerInfo
    { siType :: ServerType
    , siPid :: ProcessID
    , siPort :: Int
    , siKey :: String
    }
instance ToJSON ServerInfo where
    toJSON si = object [ typeElement (siType si)
                       , "pid" .= (fromIntegral (siPid si) :: Int)
                       , "port" .= siPort si
                       , "key" .= siKey si
                       ]
      where
        typeElement LocalServer = "local" .= True
        typeElement (RemoteServer endpoint) = "remote" .= endpoint
instance FromJSON ServerInfo where
    parseJSON (Object v) =
        ServerInfo <$> parseType
                   <*> ((fromIntegral :: Int -> ProcessID) <$> v .: "pid")
                   <*> v .: "port"
                   <*> v .: "key"
      where
        parseType = ((const LocalServer :: Bool -> ServerType) <$> v .: "local")
                    <|> (RemoteServer <$> v .: "remote")

    parseJSON _ = mzero

-- | A uniform, human readable prefix for a `ServerType`
serverTypePrefix :: ServerType -> String
serverTypePrefix LocalServer = "local plush"
serverTypePrefix (RemoteServer endpoint) = "remote to " ++ endpoint

-- | A uniform, human readable prefix for a `ServerInfo`
serverInfoPrefix :: ServerInfo -> String
serverInfoPrefix si = serverTypePrefix (siType si) ++ extra (siType si) ++ proc
  where
    extra LocalServer = ""
    extra (RemoteServer _) = ", ssh"
    proc = "[" ++ show (siPid si) ++ "]"

-- | All server information is stored in files under @~/.plush/server@. This
-- function returns that path to some named file in that directory, making the
-- directories if needed. If @HOME@ cannot be determined, then all storage
-- functions do nothing.
basePath :: FilePath -> IO (Maybe FilePath)
basePath fp = getEnv "HOME" >>= maybe (return Nothing) mkBase
  where
    mkBase home = let server = home </> ".plush/server" in do
        createPath server ownerModes
        return $ Just $ server </> fp

-- | Path to store server info in.
serverInfoPath :: ServerType ->  IO (Maybe FilePath)
serverInfoPath LocalServer =  basePath "local.json"
serverInfoPath (RemoteServer endpoint) =
    basePath ("remote-" ++ endpoint ++ ".json")

readServerInfo :: ServerType -> IO (Maybe ServerInfo)
readServerInfo st =
    serverInfoPath st >>= maybe (return Nothing) readJsonFile

writeServerInfo :: ServerType -> ServerInfo -> IO ()
writeServerInfo st si =
    serverInfoPath st >>= maybe (return ()) (writeJsonFile si)

readJsonFile :: (FromJSON a) => FilePath -> IO (Maybe a)
readJsonFile fp =
    (decode' <$> readAllFile fp) `Ex.catchIOError` (\_ -> return Nothing)

writeJsonFile :: (ToJSON a) => a -> FilePath -> IO ()
writeJsonFile a fp =
    (writeAllFile fp $ encode a) `Ex.catchIOError` (\_ -> return ())

removeServerInfo :: ServerType -> IO ()
removeServerInfo st = serverInfoPath st >>= maybe (return ()) rm
  where
    rm fp = removeLink fp `Ex.catchIOError` (\_ -> return ())

-- \ Return the list of all servers with information stored. Determined simply
-- from the `basePath` directory contents.
allKnownServers :: IO [ServerType]
allKnownServers = basePath "" >>= maybe (return []) go
  where
    go dir = mapMaybe maybeServer <$> getDirectoryContents dir
    maybeServer fname
        | fname == "local.json" = Just LocalServer
        | otherwise = RemoteServer <$>
                        (prefix "remote-" fname >>= suffix ".json")
    prefix p s | p `isPrefixOf` s = Just $ drop (length p) s
               | otherwise = Nothing
    suffix p s = reverse <$> prefix (reverse p) (reverse s)

-- | A logger is an action that will log information in the server log file
-- (@~/.plush/server/log@) The log entries are prefixed for a given server.
-- Entries are time stamped, and the log file is opened and closed on each log
-- action.
makeLogger :: ServerInfo -> IO (String -> IO ())
makeLogger si = basePath "log" >>= return . maybe nullLogger logger
  where
    nullLogger _s = return ()

    logger logPath s = safe $ Ex.bracket (openFdRW logPath) closeFd $ \fd -> do
        t <- formatTime undefinedLocale "%F %T " <$> getZonedTime
        writeStrLn fd $ t ++ serverInfoPrefix si ++ ": " ++ s

    undefinedLocale = undefined -- no locale needed by those format characters

    safe act = act `Ex.catchIOError` (\_ -> return ())  -- logging never fails
    openFdRW fp = openFd fp ReadWrite (Just ownerRWMode) defaultFileFlags
    ownerRWMode = ownerReadMode `unionFileModes` ownerWriteMode

-- | Format the url for a given server.
startUrl :: ServerInfo -> String
startUrl si =
    "http://localhost:" ++ show (siPort si) ++ "/index.html#" ++ siKey si

-- | The path format string for ssh's control pipes.
sshControlFilePath :: IO (Maybe FilePath)
sshControlFilePath = basePath "control-%r:%h:%p"
