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

module Plush.Run.BuiltIns.Plush (
    plushVersion,
    )
where

import Data.Maybe (fromMaybe)
import Data.Version (Version(..), showVersion)

import Plush.Run.BuiltIns.Utilities
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.Types
import qualified Paths_plush as CabalPaths

plushVersion :: (PosixLike m) => SpecialUtility m
plushVersion = SpecialUtility . const $ Utility exec noArgsAnnotate
  where
    exec _ = do
        outStrLn $ displayVersion CabalPaths.version
        success

    displayVersion v = fromMaybe (showVersion v) $ last' (versionTags v)
    last' [] = Nothing
    last' [a] = Just a
    last' (_:as) = last' as

