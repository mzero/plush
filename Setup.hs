module Main (main) where

import Data.Version (Version(..))
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple
    (defaultMainWithHooks, simpleUserHooks, UserHooks(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
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
    act pd' lbi uh flag
  where
    modVerTagsOfDesc = modIdOfDesc . modVerOfId . modTagsOfVer
    modTagsOfVer f ver   = ver { versionTags = f $ versionTags ver }
    modVerOfId   f pid   = pid { pkgVersion = f $ pkgVersion pid }
    modIdOfDesc  f pdesc = pdesc { package = f $ package pd }

    addDescTag = (flip (++)) . take 1 . lines

hookActionArgs :: (a -> Action f) -> a -> Action f
hookActionArgs actArgs args = hookAction (actArgs args)
