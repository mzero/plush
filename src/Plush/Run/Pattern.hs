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
    pathnameGlob,
    )
where

import Control.Monad (foldM)
import Control.Monad.Exception (catchIOError)
import Data.List (foldl', sort, tails)
import System.FilePath (combine, (</>))
import Text.Parsec

import Plush.Run.Posix
import Plush.Run.Posix.Utilities


data PatternPart = Literal Char | CharTest (Char -> Bool) | Star
type Pattern = [PatternPart]
data PatternAction = AddSlash | MatchContents Pattern
type PathPattern = [PatternAction]

pathnameGlob :: (PosixLike m) => FilePath -> String -> m [String]
pathnameGlob base = either (\_err -> return []) match . parse patP ""
  where
    match :: (PosixLike m) => PathPattern -> m [String]
    match = foldM steps [""]
      where
        steps fs p = concat `fmap` mapM (step p) fs

        step (MatchContents p) dir = do
            entries <- getDirectoryContents (ifNull "." $ base </> dir)
                `catchIOError` (\_ -> return [])
            return $ map (combine dir) $ sort $ filter (patMatch p) entries
        step (AddSlash) path = do
            isDir <- doesDirectoryExist (ifNull "/" $ base </> path)
            return $ if isDir then [path ++ "/"] else []

    ifNull d s = if null s then d else s

    patP =  many1 (slashP <|> matchP)
    slashP = char '/' >> return AddSlash
    matchP = many1 partP >>= return . MatchContents
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
        <|> (noneOf "/" >>= return . Literal)

    makeCharClass :: [Char] -> [Char]
    makeCharClass []                   = []
    makeCharClass (begin:'-':end:rest) = [begin .. end] ++ makeCharClass rest
    makeCharClass (c:cs)               = c : makeCharClass cs

    patMatch :: Pattern -> String -> Bool
    patMatch pattern s = any null $ foldl' (flip concatMap) [s] matchers
      where
        matchers = zipWith g (True:repeat False) pattern
        g _    (Literal l)  (c:cs)  = if l == c then [cs] else []
        g True _            ('.':_) = []
        g _    (CharTest p) (c:cs)  = if p c then [cs] else []
        g _    Star            cs   = tails cs
        g _    _               []   = []



