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

module Plush.Run.Pattern (
    Pattern,
    makePattern,

    shortestPrefix, longestPrefix,
    shortestSuffix, longestSuffix,
    patternMatch,
    pathnameGlob,
    )
where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.Exception (catchIOError)
import Data.List (foldl', sort, tails)
import System.FilePath (combine, (</>))
import Text.Parsec
import Text.Parsec.String

import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Types


data PatternPart = Literal Char | CharTest (Char -> Bool) | Star | NoMatch
type Pattern = [PatternPart]

pattern :: String -> Parser Pattern
pattern exclude = many1 partP
  where
    partP = (char '*' >> return Star)
        <|> (char '?' >> return (CharTest (const True)))
        <|> (char '\\' >> anyChar >>= return . Literal)
        <|> (try $ do _ <- char '['
                      c <- noneOf "/"
                      first <- if c == '!'
                               then noneOf "/"
                               else return c
                      rest <- many $ noneOf "]/"
                      _ <- char ']'
                      let f = if c == '!'
                              then flip notElem
                              else flip elem
                      return $ CharTest (f $ makeCharClass (first:rest)))
        <|> (noneOf exclude >>= return . Literal)

    makeCharClass :: [Char] -> [Char]
    makeCharClass []                   = []
    makeCharClass (begin:'-':end:rest) = [begin .. end] ++ makeCharClass rest
    makeCharClass (c:cs)               = c : makeCharClass cs

-- | Compile a string into a 'Pattern'. If the string is an invalid pattern,
-- then the resutling pattern will match no strings.
makePattern :: Word -> Pattern
makePattern = concatMap asPattern . compress . parts
  where
    asPattern (Bare s)      = strPattern s
    asPattern (Expanded s)  = strPattern s
    asPattern p             = litPattern $ partText p

    strPattern = either (\_ -> [NoMatch]) id . parse (pattern "") ""
    litPattern = map Literal

-- | Match a pattern against a string (anchored at the start), and return a
-- list of all possible remainders (after the matched part). If the @initialDot@
-- is 'True', then an initial dot in the string will only match a literal dot
-- in the pattern.
matchRemainders :: Bool -> Pattern -> String -> [String]
matchRemainders initialDot pat s = foldl' (flip concatMap) [s] matchers
  where
    matchers = zipWith g (initialDot:repeat False) pat
    g _    (Literal l)  (c:cs)  = if l == c then [cs] else []
    g True _            ('.':_) = []
    g _    (CharTest p) (c:cs)  = if p c then [cs] else []
    g _    Star            cs   = tails cs
    g _    NoMatch          _   = []
    g _    _               []   = []

-- | Test if a pattern can match the entire string with no remainders.
patternMatch :: Pattern -> String -> Bool
patternMatch pat s = any null $ matchRemainders True pat s


pickRemainder :: ([(Int, String)] -> (Int, String))
    -> Pattern -> String -> Maybe String
pickRemainder sift pat s = pickMatch $ matchRemainders False pat s
  where
    pickMatch [] = Nothing
    pickMatch rs = Just . snd . sift $ map (\r -> (length r, r)) rs

-- | Return the string with the shortest prefix match removed
shortestPrefix :: Pattern -> String -> Maybe String
shortestPrefix = pickRemainder  maximum

-- | Return the string with the longest prefix match removed
longestPrefix :: Pattern -> String -> Maybe String
longestPrefix = pickRemainder minimum

-- | Return the string with the shortest suffix match removed
shortestSuffix :: Pattern -> String -> Maybe String
shortestSuffix p s = reverse <$> shortestPrefix (reverse p) (reverse s)

-- | Return the string with the longest suffix match removed
longestSuffix :: Pattern -> String -> Maybe String
longestSuffix p s = reverse <$> longestPrefix (reverse p) (reverse s)



data PatternAction = AddSlash | MatchContents Pattern
type PathPattern = [PatternAction]

pathnamePattern :: Parser PathPattern
pathnamePattern = many1 (slashP <|> matchP)
  where
    slashP = char '/' >> return AddSlash
    matchP = pattern "/" >>= return . MatchContents


pathnameGlob :: (PosixLike m) => FilePath -> String -> m [String]
pathnameGlob base = either (\_err -> return []) match . parse pathnamePattern ""
  where
    match :: (PosixLike m) => PathPattern -> m [String]
    match = foldM steps [""]
      where
        steps fs p = concat `fmap` mapM (step p) fs

        step (MatchContents p) dir = do
            entries <- getDirectoryContents (ifNull "." $ base </> dir)
                `catchIOError` (\_ -> return [])
            return $ map (combine dir) $ sort $ filter (globMatch p) entries
        step (AddSlash) path = do
            isDir <- doesDirectoryExist (ifNull "/" $ base </> path)
            return $ if isDir then [path ++ "/"] else []

    ifNull d s = if null s then d else s

    globMatch pat = any null . matchRemainders True pat
