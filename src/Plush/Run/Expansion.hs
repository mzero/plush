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

module Plush.Run.Expansion (
    expandActive,
    expandPassive,
    expandAndSplit,
    wordExpansionActive,
    TildePart, tildeDir, tildePrefix,
    tildeSplit,
    quoteRemoval,
    byPathParts,
    )
where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.List (intercalate, partition)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import Text.Parsec

import Plush.Run.Pattern
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
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
wordExpansion live w = tildeExpansion w >>= modifyPartsM (mapM we)
    -- TODO: honor the live parameter
  where
    we (Parameter name pmod) = parameterExpansion live name pmod
    -- we (Subcommand cl) = return $ Expanded (testExecute cl)
    -- TODO: arthmetic
    we (Doublequoted dw) = mapM we dw >>= return . Doublequoted
    we p = return p

parameterExpansion :: (PosixLike m) =>
    Bool -> String -> ParameterModifier -> ShellExec m WordPart
parameterExpansion live name pmod = getVar name >>= fmap Expanded . pmodf pmod
  where
    pmodf PModNone                 = return . fromMaybe ""
    pmodf (PModUseDefault t wd)    = pAct (ptest t) wd return
    pmodf (PModAssignDefault t wd) = pAct (ptest t) wd setVar
    pmodf (PModIndicateError t wd) = pAct (ptest t) wd (indicateError t)
    pmodf (PModUseAlternate t wd)  = pAct (not . ptest t) wd return
    pmodf PModLength               = return . show . length . fromMaybe ""
    pmodf (PModRemovePrefix False wd) = remove shortestPrefix wd
    pmodf (PModRemovePrefix True  wd) = remove longestPrefix wd
    pmodf (PModRemoveSuffix False wd) = remove shortestSuffix wd
    pmodf (PModRemoveSuffix True  wd) = remove longestSuffix wd

    getWord wd = wordExpansion live wd

    pAct f wd act v = if f v
        then return $ fromMaybe "" v
        else getWord wd >>= act . wordText

    ptest _     Nothing  = False
    ptest False (Just _) = True         -- plain mods use any assigned value
    ptest True  (Just s) = not $ null s -- ':' mods only use non null values

    setVar s = setVarEntry name (VarShellOnly, VarReadWrite, Just s) >> return s

    remove f wd = maybe (return "") $ \s ->
        getWord wd >>= return . fromMaybe s . flip f s . makePattern

    indicateError True ""  = shouldError " is unset or null"
    indicateError False "" = shouldError " is unset"
    indicateError _ msg    = shouldError $ ": " ++ msg

    shouldError msg = errStrLn (name ++ msg) >> return ""
        -- TODO(mzero): should shell error at this point



tildeExpansion :: (PosixLike m) => Word -> ShellExec m Word
tildeExpansion = modifyPartsM $ go . compress
  where
    go [Bare s] = (:[]) . Bare . te <$> tildeSplit s
    go ps = return ps

    te (tildePart, pathPart) = (maybe id (</>) $ tildeDir tildePart) pathPart
        -- Bare, not Singlequoted nor Expanded because the results of tilde
        -- expansion are not subject to field splitting, but are subject to
        -- pathname expansion.

type TildePart = Maybe (String, FilePath)

tildeDir :: TildePart -> Maybe FilePath
tildeDir = fmap snd

tildePrefix :: TildePart -> Maybe String
tildePrefix = fmap fst

-- | Break a string into the tilde prefix, if any, and the remainder.
-- The prefix is returned along with its expansion. If the string starts with a
-- tilde, but the named user doesn't exist, then it isn't treated as a tilde
-- prefix, and 'Nothing' is returned for the prefix.
tildeSplit :: (PosixLike m) => String
    -> ShellExec m (TildePart, String)
tildeSplit s@('~':t) = do
    let (name,rest) = break (== '/') t
    dir <- if null name
                then getVar "HOME"
                else getUserHomeDirectoryForName name
    case dir of
        Nothing -> return (Nothing, s)
        Just d -> return (Just ('~':name, d), drop 1 rest)
            -- Note: because of break, rest is either empty or starts with '/'
tildeSplit s = return (Nothing, s)



data SeparatorType = Whitespace | Explicit
data FieldPart = Separator SeparatorType | FieldPart WordPart

fieldSplitting :: String -> Word -> [Word]
fieldSplitting ifs = expandParts $
    split [] . ignoreFirst . concatMap breakup . compress
  where
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
pathnameExpansion w = results <$> pathnameGlob "" (parts w)
    -- TODO: should skip pathnameExpansion of shell flag -f is set
  where
    results [] = [w]
    results es = expandParts (const [[Bare e] | e <- es]) w


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
