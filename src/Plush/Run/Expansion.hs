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

module Plush.Run.Expansion (
    expandActive,
    expandPassive,
    expandAndSplit,
    wordExpansionActive,
    pathnameGlob,
    quoteRemoval,
    byPathParts,
    )
where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.Exception (catchIOError)
import Data.Char (isSpace)
import Data.List (foldl', intercalate, partition, sort, tails)
import System.FilePath (combine)
import Text.Parsec

import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Types

-- | Like 'expandActive', but preforms the final quote removal as well
expandAndSplit :: (PosixLike m) => [Word] -> ShellExec m [String]
expandAndSplit w = map quoteRemoval <$> expandActive w

-- | Peform all shell expansions, in order:
--
-- * tilde expansion
-- * parameter expansion
-- * arithmetic expansion (not yet implemented)
-- * subcommand expansion (not yet implemented)
-- * field splitting
-- * pathname expansion
expandActive :: (PosixLike m) => [Word] -> ShellExec m [Word]
expandActive = expand True

-- | Like 'expandActive', only sub commands are not run, nor any shell variables
-- modified.
expandPassive :: (PosixLike m) => [Word] -> ShellExec m [Word]
expandPassive = expand False

expand :: (PosixLike m) => Bool -> [Word] -> ShellExec m [Word]
expand live w = do
    wWordExp <- mapM (wordExpansion live) w
    ifs <- getVarDefault "IFS" " \t\n"
    let wFieldSplit = concatMap (fieldSplitting ifs) wWordExp
    concat `fmap` mapM pathnameExpansion wFieldSplit


wordExpansionActive :: (PosixLike m) => Word -> ShellExec m Word
wordExpansionActive = wordExpansion True

wordExpansion :: (PosixLike m) => Bool -> Word -> ShellExec m Word
wordExpansion _live w = tildeExpansion w >>= modifyPartsM (mapM we)
    -- TODO: honor the live parameter
  where
    we (Parameter name Nothing) = Expanded `fmap` getVarDefault name ""
    -- TODO: parameters with modifiers
    -- we (Subcommand cl) = return $ Expanded (testExecute cl)
    -- TODO: arthmetic
    we (Doublequoted dw) = mapM we dw >>= return . Doublequoted
    we p = return p


tildeExpansion :: (PosixLike m) => Word -> ShellExec m Word
tildeExpansion w0 = case w0 of
    Word l (Bare ('~':cs):wz) -> maybe w0 (Word l) <$> te "" ((Bare cs):wz)
    _ -> return w0
  where
    te n ((Bare s):ws) = case break (== '/') s of
        (r,[]) -> te (n++r) ws
        (r,q)  -> ex (n++r) >>= onto (Bare q:ws)
    te n [] = ex n >>= onto []
    te _ _ = return Nothing

    ex "" = getVar "HOME"
    ex n = getUserHomeDirectoryForName n

    onto ws = return . fmap (\e -> Bare e : ws)
        -- Bare, not Singlequoted nor Expanded because the results of tilde
        -- expansion are not subject to field splitting, but are subject to
        -- pathname expansion.


data SeparatorType = Whitespace | Explicit
data FieldPart = Separator SeparatorType | FieldPart WordPart

fieldSplitting :: String -> Word -> [Word]
fieldSplitting ifs = expandParts $
    split [] . ignoreFirst . concatMap breakup . compress
  where
    compress ((Expanded s):(Expanded t):ps) = compress $ (Expanded (s++t)) : ps
    compress (p:ps) = p : compress ps
    compress [] = []

    breakup (Expanded s) = either err id $ parse fieldP "" s
    breakup p = [FieldPart p]

    fieldP = many (sepWhP <|> sepExP <|> bareP)
    sepWhP = skipMany1 ifsWhite >> (sepExP <|> return (Separator Whitespace))
    sepExP = ifsSeparator >> skipMany ifsWhite >> return (Separator Explicit)
    bareP = (FieldPart . Bare) `fmap` many1 nonIfs
    err e = [FieldPart (Bare ("### INTERNAL FIELD SPLIT ERROR ### " ++ show e))]

    ignoreFirst ((Separator Whitespace) : ps) = ps
    ignoreFirst ps = ps

    split w ((Separator _) : ps) = w : split [] ps
    split w ((FieldPart p) : ps) = split (w++[p]) ps
    split [] [] = []
    split w [] = [w]

    (ifsWhiteChars, ifsSeparatorChars) = partition isSpace ifs
    ifsWhite = oneOf ifsWhiteChars
    ifsSeparator = oneOf ifsSeparatorChars
    nonIfs = noneOf ifs




pathnameExpansion :: (PosixLike m) => Word -> m [Word]
pathnameExpansion =  origIfNull $ (expandPartsM $ glob . compress)
    -- TODO: should skip pathnameExpansion of shell flag -f is set
  where
    compress :: Parts -> Parts
    compress ((Bare s):(Bare t):ps) = compress $ Bare (s ++ t) : ps
    compress (p:ps) = p : compress ps
    compress [] = []

    glob :: (PosixLike m) => Parts -> m [Parts]
    glob [(Bare s)] = pathnameGlob s >>= return . map ((:[]).Bare)
    glob _ = return []

    origIfNull f a = (\bs -> if null bs then [a] else bs) <$> f a


data PatternPart = Literal Char | CharTest (Char -> Bool) | Star
type Pattern = [PatternPart]
data PatternAction = AddSlash | MatchContents Pattern
type PathPattern = [PatternAction]

pathnameGlob :: (PosixLike m) => String -> m [String]
pathnameGlob = either (\_err -> return []) match . parse patP ""
  where
    match :: (PosixLike m) => PathPattern -> m [String]
    match = foldM steps [""]
      where
        steps fs p = concat `fmap` mapM (step p) fs

        step (MatchContents p) dir = do
            entries <- getDirectoryContents (if null dir then "." else dir)
                `catchIOError` (\_ -> return [])
            return $ map (combine dir) $ sort $ filter (patMatch p) entries
        step (AddSlash) path = do
            isDir <- doesDirectoryExist (if null path then "/" else path)
            return $ if isDir then [path ++ "/"] else []

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



quoteRemoval :: Word -> String
quoteRemoval = concatMap qp . parts
  where
    qp (Backslashed '\n') = ""  -- should this be handled here or in the parse?
    qp (Backslashed c) = [c]
    qp (Singlequoted s) = s
    qp (Doublequoted ps) = concatMap qp ps
    qp p = partText p


byPathParts :: (PosixLike m) => (Word -> ShellExec m Word)
            -> Word -> ShellExec m Word
byPathParts f w = unparts <$> mapM f (expandParts (byparts []) w)
  where
    byparts [] [] = []
    byparts acc [] = reverse acc : []
    byparts acc (Bare s:ws) = case break (== ':') s of
        (r,(':':q)) -> reverse (Bare r:acc) : byparts [] (Bare q:ws)
        _ -> byparts (Bare s:acc) ws
    byparts acc (x:ws) = byparts (x:acc) ws

    unparts = Word (location w) . intercalate [Bare ":"] . map parts



