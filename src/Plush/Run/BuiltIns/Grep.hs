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

module Plush.Run.BuiltIns.Grep (
    egrep,
    fgrep,
    grep,
    )
where

import Control.Applicative ((<$>))
import Control.Monad (when)
import qualified Data.Text.Lazy as T

import Plush.Run.BuiltIns.Syntax
import Plush.Run.BuiltIns.Utilities
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.Types

egrep :: (PosixLike m) => BuiltInUtility m
egrep = BuiltInUtility $ stdSyntax baseOptions "E" doGrep

fgrep :: (PosixLike m) => BuiltInUtility m
fgrep = BuiltInUtility $ stdSyntax baseOptions "F" doGrep

grep :: (PosixLike m) => BuiltInUtility m
grep = BuiltInUtility $ stdSyntax (patternOptions ++ baseOptions) "" doGrep

baseOptions =
    [ toggle 'c' "clq"  -- write only a count of selected lines
    , toggle 'l' "clq"  -- write only the names of matching files
    , toggle 'q' "clq"  -- quiet mode: no output, only exit status
    , flag 'i'  -- case-insensitive matching
    , flag 'n'  -- include line numbers
    , flag 's'  -- supress error messages, TODO: implement s flag
    , flag 'v'  -- invert matching
    , flag 'x'  -- match whole line
    ]

patternOptions =
    [ toggle 'E' "EF"   -- extended regular expressions
    , toggle 'F' "EF"   -- fixed strings
    ]

doGrep :: (PosixLike m) => String -> Args -> m ExitCode
doGrep _ [] = exitMsg 1 "no pattern provided"
doGrep flags (pat:args) = do
    if not ('F' `elem` flags)
        then exitMsg 2 "only -F (fgrep) is currently supported"
        else do
            found <- case args of
                []     -> gofile False "-"
                [file] -> gofile False file
                files  -> or <$> mapM (gofile True) files
                    -- TODO: should short-circuit
            if found then success else failure
  where
    gofile df "-" = readAll stdInput >>= gotext df "(standard input)"
    gofile df f = readAllFile f >>= gotext df f

    gotext df f contents = do
        let matches = filter (match . snd)
                        $ zip [(1::Int)..] $ T.lines $ fromByteString contents
        when showLines $ mapM_ (dispLine df f) matches
        dispFile df f matches
        return $ not $ null matches

    match = sense . test (encase $ T.pack pat) . encase
    sense = if 'v' `elem` flags then not else id
    encase = if 'i' `elem` flags then T.toLower else id
    test = if 'x' `elem` flags then (==) else T.isInfixOf

    dispLine df f (n,s) =
        outStrLn $ (filePrefix df f) ++ (lineNumber n) ++ T.unpack s

    (showLines, dispFile) = case filter (`elem` "clq") flags of
        "c" -> (False, dispFileC)
        "l" -> (False, dispFileL)
        "q" -> (False, dispFileQ)
        _ -> (True, dispFileQ)

    dispFileC df f m = outStrLn $ (filePrefix df f) ++ show (length m)
    dispFileL _ f m = when (not $ null m) $ outStrLn f
    dispFileQ _ _ _ = return ()

    filePrefix True f = f ++ ":"
    filePrefix False _ = ""

    lineNumber n = if 'n' `elem` flags then show n ++ ":" else ""
