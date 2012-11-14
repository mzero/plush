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

{-# LANGUAGE CPP, TemplateHaskell #-}

module Plush.Utilities (
    readUtf8File,
    getDataResource,
    getStaticResource,
    )
    where

import qualified Data.ByteString as B
#ifdef PRODUCTION
import Data.FileEmbed
#endif
import System.Directory (getCurrentDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO
import System.Posix (getEnv)

import qualified Paths_plush as CabalPaths


-- | Lazily get a text file's contents as with 'readFile', but assume its
-- contents are encoded with UTF-8 regardless of the user's current locale.
readUtf8File :: FilePath -> IO String
readUtf8File path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    hGetContents h


getDataResource :: FilePath -> IO (Maybe B.ByteString)
getStaticResource :: FilePath -> IO (Maybe B.ByteString)

#ifdef PRODUCTION

getDataResource = return . flip lookup dataResources
getStaticResource = return . flip lookup staticResources

dataResources :: [(FilePath, B.ByteString)]
dataResources = $(embedDir "data")

staticResources :: [(FilePath, B.ByteString)]
staticResources = $(embedDir "static")

#else

getDataResource = getResource . ("data" </>)
getStaticResource = getResource . ("static" </>)

getResource :: FilePath -> IO (Maybe B.ByteString)
getResource fp = do
    fp' <- (</> fp) `fmap` getDataDir
    exists <- doesFileExist fp'
    if exists
        then Just `fmap` B.readFile fp'
        else return Nothing

#endif

#ifdef PRODUCTION
-- | Return the a data directory where Plush's static data files are installed.
-- Can be overriden by the environment variable @plush_datadir@.
getDataDir :: IO FilePath
getDataDir = CabalPaths.getDataDir
#else
-- | Return the a data directory where Plush's static data files are installed.
-- Can be overriden by the environment variable @plush_datadir@.
-- For non-production builds, this is based on the 'getDataDir' that Cabal
-- generates, only the current directory is used as a last resort.
getDataDir :: IO FilePath
getDataDir = do
    me <- getEnv "plush_datadir"
    case me of
        Just e -> return e
        Nothing -> do
            d <- CabalPaths.getDataDir
            de <- doesDirectoryExist d
            if de
                then return d
                else getCurrentDirectory
#endif
