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

module Plush.Run.Redirection (
    withRedirection
    )
    where

import Control.Applicative ((<$>))
import Control.Monad.Exception (bracket, catchIOError)
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)

import Plush.Run.Expansion
import Plush.Run.Posix
import Plush.Run.Posix.Utilities (errStr, errStrLn)
import Plush.Run.ShellExec
import Plush.Run.Types
import Plush.Types



type OpenOptions = (OpenMode, Maybe FileMode, OpenFileFlags)

data RedirPrim
    = RedirFile Fd FilePath OpenOptions
--  | RedirHere Fd String -- not yet supported
    | RedirDup Fd Fd
    | RedirClose Fd

mkPrim :: (PosixLike m) => Redirect -> ShellExec m (Either String RedirPrim)
mkPrim (Redirect maybeFd rType w) = wordExpansionActive w >>= mk . quoteRemoval
  where
    fd = Fd . fromIntegral $ fromMaybe (defaultFd rType) maybeFd

    defaultFd RedirOutput = 1
    defaultFd RedirOutputClobber = 1
    defaultFd RedirAppend = 1
    defaultFd RedirDuplicateOutput = 1
    defaultFd _ = 0

    mk s =
      case rType of
        RedirInput         -> ok $ RedirFile fd s openForRead
        RedirOutput        -> ok $ RedirFile fd s openForWrite
        RedirOutputClobber -> ok $ RedirFile fd s openForWrite
        RedirAppend        -> ok $ RedirFile fd s openForAppend
        RedirInputOutput   -> ok $ RedirFile fd s openForAll
            -- TODO: RedirOutput should check that file doesn't exist

        RedirDuplicateInput  | isJust dest -> ok $ RedirDup fd (fromJust dest)
        RedirDuplicateInput  | hasDash     -> ok $ RedirClose fd
        RedirDuplicateInput  | otherwise   -> err $ "bad redirect"
        RedirDuplicateOutput | isJust dest -> ok $ RedirDup fd (fromJust dest)
        RedirDuplicateOutput | hasDash     -> ok $ RedirClose fd
        RedirDuplicateOutput | otherwise   -> err $ "bad redirect"

      where
        hasDash = s == "-"
        dest = listToMaybe [ Fd i | (i,r) <- reads s, null r]

    ok = return . Right
    err = return . Left

    openForRead   = (ReadOnly, Nothing, defaultFileFlags)
    openForWrite  = (WriteOnly, Just stdFileMode, truncFileFlags)
    openForAppend = (WriteOnly, Just stdFileMode, appendFileFlags)
    openForAll    = (ReadWrite, Just stdFileMode, defaultFileFlags)

    truncFileFlags = defaultFileFlags { trunc = True }
    appendFileFlags = defaultFileFlags { append = True }

mkPrim (RedirectHere maybeFd hereDoc w) = debugHereDoc >>
    return (Left "here docs not supported")
        -- TODO(mzero): implement here docs for real
  where
    debugHereDoc = do
        errStrLn divider
        errStr $ (maybe "" show maybeFd) ++ "<<" ++ wordText w
        errStr . unlines $ case hereDoc of
            HereDocLiteral ls -> " (literal)" : ls
            HereDocParsed ls -> " (parsed)" : ls
            HereDocMissing -> " (missing)" : []
        errStrLn divider
    divider = "~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~"

withRedirection :: (PosixLike m) => [Redirect] -> ShellExec m ExitCode
    -> ShellExec m ExitCode
withRedirection [] act = act
withRedirection (r:rs) act = do
    errOrPrim <- mkPrim r
    case errOrPrim of
        Left err -> exitMsg 125 err

        Right (RedirFile destFd fp (om, mfm, off)) -> saveAway destFd $ do
            fileFd <- openFd fp om mfm off
            dupTo fileFd destFd
            closeFd fileFd
            withRedirection rs act

        Right (RedirDup destFd srcFd) -> saveAway destFd $ do
            dupTo srcFd destFd
            withRedirection rs act

        Right (RedirClose destFd) -> saveAway destFd $ do
            safeCloseFd destFd
            withRedirection rs act

  where
    saveAway destFd inner = bracket
        ((Just <$> dupFdCloseOnExec destFd safeFdArea)
                    `catchIOError` (\_ -> return Nothing))
        (\moved -> case moved of
            Just savedFd -> dupTo savedFd destFd >> closeFd savedFd
            Nothing      -> safeCloseFd destFd
            )
        (const inner)

    safeCloseFd fd = closeFd fd `catchIOError` (\_ -> return ())
    safeFdArea = 30 -- minimum Fd to use for storing away Fds

