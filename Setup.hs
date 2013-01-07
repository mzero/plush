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

-- TL;DR: To get a profilng build, use:
--              cabal configure --enable-executable-profiling

module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Version (Version(..))
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple
    (defaultMainWithHooks, simpleUserHooks, UserHooks(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup (ConfigFlags(..))
import System.Process (readProcess)

main :: IO ()
main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks
                { buildHook   = hookAction $ buildHook simpleUserHooks
                , haddockHook = hookAction $ haddockHook simpleUserHooks
                , testHook    = hookAction $ testHook simpleUserHooks
                , benchHook   = hookActionArgs $ benchHook simpleUserHooks
                }

type Action f = PackageDescription -> LocalBuildInfo -> UserHooks -> f -> IO ()

hookAction :: Action f -> Action f
hookAction act pd lbi uh flag = do
    desc <- readProcess "git" (words "describe --always --dirty") ""
    let pd' = modVerTagsOfDesc (addDescTag desc) $ pd
    act pd' lbi' uh flag
  where
    modVerTagsOfDesc = modIdOfDesc . modVerOfId . modTagsOfVer
    modTagsOfVer f ver   = ver { versionTags = f $ versionTags ver }
    modVerOfId   f pid   = pid { pkgVersion = f $ pkgVersion pid }
    modIdOfDesc  f pdesc = pdesc { package = f $ package pd }

    addDescTag = (flip (++)) . take 1 . lines

    profiling = withProfExe lbi
    lbi' = lbi { withProfLib = profiling, withProfExe = profiling }
        -- This forces proiling all on or all off based on the exe profiling
        -- setting. This overrrides whatever the user may have set in
        -- .cabal/config or on the command line during cabal configure.


hookActionArgs :: (a -> Action f) -> a -> Action f
hookActionArgs actArgs args = hookAction (actArgs args)
