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

module Plush.Utilities (
    readMaybe,
    readUtf8File,
    displayVersion,
    )
    where

import Data.Maybe (listToMaybe)
import Data.Version (Version(..), showVersion)
import System.IO

import qualified Paths_plush as CabalPaths


-- | The missing read function. The string must parse entirely for this to
-- return a value.
readMaybe :: (Read a) => String -> Maybe a
readMaybe = listToMaybe . map fst . filter (null . snd) . reads

-- | Lazily get a text file's contents as with 'readFile', but assume its
-- contents are encoded with UTF-8 regardless of the user's current locale.
readUtf8File :: FilePath -> IO String
readUtf8File path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    hGetContents h


displayVersion :: String
displayVersion = headOr (showVersion v) $ reverse $ versionTags v
  where
    v = CabalPaths.version
    headOr def [] = def
    headOr _ (a:_) = a
