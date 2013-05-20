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

{-# LANGUAGE CPP, TemplateHaskell #-}

module Plush.Resource (
    getDataResource,
    getStaticResource,
    )
    where

import qualified Data.ByteString as B

#ifndef LIVE_FILES
import Data.FileEmbed
#else
import System.Directory (getCurrentDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.Posix (getEnv)

import qualified Paths_plush as CabalPaths
#endif


getDataResource :: FilePath -> IO (Maybe B.ByteString)
getStaticResource :: FilePath -> IO (Maybe B.ByteString)

#ifndef LIVE_FILES

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
