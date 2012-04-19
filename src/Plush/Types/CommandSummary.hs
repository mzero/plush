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

{-# LANGUAGE OverloadedStrings #-}

module Plush.Types.CommandSummary (
    CommandSummary(..),
    parseSummaries,
    formatOptions,
    findOptionDescription,
    ) where

import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Text as T

-- | Short description of a command and its options used by the interactive
-- features of plush. It is used primarily during command entry, to provide
-- annotations about the command. This information is not intended to replace a
-- man page.
--
-- Built in utilities also rely on this information to provide the usual
-- diagnostic output on stderr.
--
-- For each option, there is a list of options (synonyms), with their leading
-- dash (possibly followed by an optional argument place-holder), and finally
-- the description.
data CommandSummary = CommandSummary
    { ciName :: T.Text          -- ^ "foo - whips text into a lather"
    , ciSynopsis :: T.Text      -- ^ "foo [-f] [-g n] [-a|-x|-h] file ..."
    , ciOptions :: [([T.Text], T.Text)] -- ^ options
    }

-- | Summaries are kept in a text file, @summaries.txt@ in plush's data dir.
-- Each command summary is just three parts: The "name" line, the "synopsis"
-- line(s), and then lines for the options. See the file for more details on
-- the formatting.
parseSummaries :: T.Text -> [(String, CommandSummary)]
parseSummaries = catMaybes . map build . splitOn divider . map T.strip . T.lines
  where
    build t = case coallesce $ splitOn T.null t of
        []       -> Nothing
        [n]      -> build' n T.empty []
        [n,s]    -> build' n s []
        (n:s:os) -> build' n s os
    build' n s os = Just (name n, CommandSummary n s (opts os))

    name = T.unpack . T.concat . take 1 . T.words

    opts os = catMaybes $ map opt $ concatMap T.lines os
    opt o | T.null o = Nothing
          | otherwise = let (flags, desc) = T.breakOn "  " o
                        in Just (T.splitOn ", " flags, T.stripStart desc)

    splitOn p = splitOn' []
      where
        splitOn' [] [] = []
        splitOn' r [] = reverse r : []
        splitOn' r (a:as) | p a = reverse r : splitOn' [] as
                          | otherwise = splitOn' (a:r) as

    coallesce ([]:as) = coallesce as
    coallesce (a:as) = T.strip (T.unlines a) : coallesce as
    coallesce [] = []

    divider t = not (T.null t) && T.all (== '-') t

-- | Prepare the options summary as a series of lines, nicely formatted for
-- textual output.
formatOptions :: CommandSummary -> T.Text
formatOptions (CommandSummary _ _ opts) =
    T.unlines $ zipWith line flags'' descs
  where
    (flags, descs) = unzip opts
    flags' = map (T.intercalate ", ") flags
    n = maximum $ map T.length flags'
    flags'' = map (T.justifyLeft n ' ') flags'
    line f d = T.concat ["  ", f, "    ", d]

-- | Find an option description in a summary, if there.
findOptionDescription :: String -> CommandSummary -> Maybe T.Text
findOptionDescription a (CommandSummary _ _ opts) =
    listToMaybe $ map snd $ filter (any match . fst) opts
  where
    a' = T.pack a
    a'' = a' `T.snoc` ' '
    match f = f == a' || a'' `T.isPrefixOf` f
