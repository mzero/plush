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

module Plush.Run.BuiltIns.FileSystem (
    mkdir,
    touch,
    rm,
    cat
    )
where

import Control.Monad (when)
import Control.Monad.Exception (bracket, catchIOError)
import System.FilePath

import Plush.Run.BuiltIns.Syntax
import Plush.Run.BuiltIns.Utilities
import Plush.Run.Posix
import Plush.Run.Types


mkdir :: (PosixLike m) => BuiltInUtility m
mkdir = BuiltInUtility $ stdSyntax options "" (perArg go)
  where
    options =
        [ flag 'p'  -- create missing directories
        -- TODO: support the -m mode option
        ]

    go flags dir = (if 'p' `elem` flags then mkPath else mkNorm) dir >> success

    mkNorm fp = createDirectory fp accessModes
    mkPath fp = do
        s <- getFileStatus fp
        when (not $ isDirectory s) $ do
            mkPath (takeDirectory fp)
            mkNorm fp


touch :: (PosixLike m) => BuiltInUtility m
touch = BuiltInUtility $ stdSyntax options "" (perArg go)
  where
    options =
        [ flag 'a'  -- set access time
        , flag 'c'  -- do not create the file if missing
        , flag 'm'  -- set modify time"
        ]

    go _flags fp = update `catchIOError` (\_ -> create) >> success
      where
        update = getFileStatus fp >> touchFile fp
            -- should respect the -a and -m flags
        create =  createFile fp stdFileMode >>= closeFd


rm :: (PosixLike m) => BuiltInUtility m
rm = BuiltInUtility $ stdSyntax options "" (perArg go)
  where
    options =
        [ toggle 'f' "fi"   -- force
        , toggle 'i' "fi"   -- interacive
        , flagAlt "rR"      -- recursive delete
        ]
        -- TODO: implement -i correctly
        -- TODO: implement proper messaging

    go flags fp = (getFileStatus fp >>= doRm)
                    `catchIOError` (\_ -> reportMissing)
      where
        doRm s = case (isDirectory s, 'r' `elem` flags) of
            (True, False) -> exitMsg 1 (fp ++ ": is a directory")
            (True, True) -> doRecRm >> removeDirectory fp >> success
            (False, _) -> removeLink fp >> success

        doRecRm = getDirectoryContents fp >>= mapM goRec >>= return . maximum
          where
            goRec entry = if notSpecial entry
                then go flags (fp </> entry)
                else success
            notSpecial = not . (`elem` [".",".."])

        reportMissing = do
            when (not $ 'f' `elem` flags) $
                errStrLn $ fp ++ ": No such file or directory"
            failure


cat :: (PosixLike m) => BuiltInUtility m
cat = BuiltInUtility $ stdSyntax options "" go
  where
    options = [ flag 'u' ] -- write without delay
        -- TODO: -u has no effect

    go _flags [] = go _flags ["-"]
    go _flags args = mapM_ (\fp -> get fp >>= write stdOutput) args >> success
    get "-" = readAll stdInput
    get fp = bracket
        (openFd fp ReadOnly Nothing defaultFileFlags)
        closeFd
        readAll
