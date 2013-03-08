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
    Expansion,
    runExpansion,
    evalExpansion,
    withExpansion,
    withExpansion',

    expandActive,
    expandPassive,
    expandAndSplit,
    wordExpansionActive,

    TildePart, tildeDir, tildePrefix,
    tildeSplit,

    byPathParts,

    contentExpansion,
    )
where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace)
import Data.List (intercalate, intersperse, partition)
import Data.Maybe (fromMaybe, maybeToList)
import System.FilePath ((</>))
import Text.Parsec

import Plush.Parser
import Plush.Run.Expansion.Arithmetic
import Plush.Run.Expansion.Types
import Plush.Run.Pattern
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import {-# SOURCE #-} Plush.Run.Execute (execute)   -- see Execute.hs-boot
import Plush.Run.Script (runScript)
import Plush.Run.ShellExec
import Plush.Run.Types
import Plush.Types


-- | Like 'expandActive', but preforms the final quote removal as well
expandAndSplit :: (PosixLike m) => [Word] -> Expansion m [String]
expandAndSplit w = map quoteRemoval <$> expandActive w

-- | Peform all shell expansions, in order:
--
-- * tilde expansion
-- * parameter expansion
-- * arithmetic expansion (not yet implemented)
-- * subcommand expansion (not yet implemented)
-- * field splitting
-- * pathname expansion
expandActive :: (PosixLike m) => [Word] -> Expansion m [Word]
expandActive = expand True

-- | Like 'expandActive', only sub commands are not run, nor any shell variables
-- modified.
expandPassive :: (PosixLike m) => [Word] -> ShellExec m [Word]
expandPassive = evalExpansion . expand False

expand :: (PosixLike m) => Bool -> [Word] -> Expansion m [Word]
expand live w = do
    wWordExp <- mapM (wordExpansion live) w
    ifs <- lift $ getVarDefault "IFS" " \t\n"
    let wFieldSplit = concatMap (fieldSplitting ifs) wWordExp
    lift $ concat `fmap` mapM pathnameExpansion wFieldSplit


wordExpansionActive :: (PosixLike m) => Word -> Expansion m Word
wordExpansionActive = wordExpansion True

wordExpansion :: (PosixLike m) => Bool -> Word -> Expansion m Word
wordExpansion live w =
    tildeExpansion w >>= modifyPartsM (fmap concat . mapM (basicExpansion live))

basicExpansion :: (PosixLike m) => Bool -> WordPart -> Expansion m [WordPart]
basicExpansion live = be
  where
    be (Parameter name pmod) = parameterExpansion live name pmod
    be (Commandsub cmd) | live = commandSubstituion (execute cmd) >>= expanded
    be (Backquoted s)   | live = commandSubstituion (runScript s) >>= expanded
    be (Arithmetic aw) = beAll aw >>= arithmeticExpansion >>= expanded
    be (Doublequoted dw) = beAll dw >>= doublequoted
    be p = return [p]

    beAll = fmap concat . mapM be
    expanded x = return [Expanded x]
    doublequoted [] = return []
    doublequoted qs = return [Doublequoted qs]


parameterExpansion :: (PosixLike m) =>
    Bool -> String -> ParameterModifier -> Expansion m [WordPart]
parameterExpansion live name pmod
    | name == "@" = lift getArgs       >>= pmodf               >>= asFields
    | name == "*" = lift getArgs       >>= pmodf               >>= asParts
    | otherwise   = lift (getVar name) >>= pmodf . maybeToList >>= asParts
  where
    pmodf = case pmod of
        PModNone                    -> return
        (PModUseDefault t wd)       -> pAct (pTest t) wd return
        (PModAssignDefault t wd)    -> pAct (pTest t) wd setVar
        (PModIndicateError t wd)    -> pAct (pTest t) wd (indicateError t)
        (PModUseAlternate t wd)     -> pAct (not . pTest t) wd return
        PModLength                  -> fmap ((:[]) . show) . fullLength
        (PModRemovePrefix False wd) -> remove shortestPrefix wd
        (PModRemovePrefix True  wd) -> remove longestPrefix wd
        (PModRemoveSuffix False wd) -> remove shortestSuffix wd
        (PModRemoveSuffix True  wd) -> remove longestSuffix wd

    pAct f wd act vs = if f vs
        then return vs
        else getWord wd >>= act . wordText >>= return . (:[])

    pTest _     [] = False
    pTest False _  = True                   -- plain mods use any assigned value
    pTest True  vs = any (not . null) vs    -- ':' mods only use non null values

    setVar s = do
        when live $ lift (setShellVar name s) >>= noteError
        return s

    indicateError True ""  = shouldError " is unset or null"
    indicateError False "" = shouldError " is unset"
    indicateError _ msg    = shouldError $ ": " ++ msg

    shouldError msg = do
        when live $ noteShellError 123 (name ++ msg)
        return ""

    fullLength [] = return 0
    fullLength [s] = return $ length s
    fullLength vs = do
        ifs <- lift getIFS
        return $ sum (map length vs) + (length vs - 1) * length ifs

    remove _ _  [] = return []
    remove f wd vs = do
        pat <- makePattern <$> getWord wd
        return $ map (\s -> fromMaybe s $ f pat s) vs

    getWord = wordExpansion live
    getIFS = take 1 <$> getVarDefault "IFS" " "

    asFields = returnParts True IFS
    asParts = returnParts False Expanded

    returnParts emptyOk _ []  = return $ if emptyOk then [] else [Expanded ""]
    returnParts _       _ [v] = return [Expanded v]
    returnParts _       f vs  = do
        ifs <- f <$> lift getIFS
        return $ intersperse ifs $ map Expanded vs


arithmeticExpansion :: (PosixLike m) => [WordPart] -> Expansion m String
arithmeticExpansion = runArithmetic . concat . map partText


commandSubstituion :: (PosixLike m) =>
    ShellExec m ShellStatus -> Expansion m String
commandSubstituion act = do
    (ec, s) <- lift (captureStdout $ statusExitCode <$> act)
    noteExitCode ec
    return $ trim "" $ fromByteString s
  where
    trim _ "" = ""
    trim nls (c:cs)
        | c == '\n' = trim (c:nls) cs
        | otherwise = nls ++ c : trim "" cs


tildeExpansion :: (PosixLike m) => Word -> Expansion m Word
tildeExpansion = modifyPartsM $ go . compress
  where
    go [Bare s] = (:[]) . Bare . te <$> lift (tildeSplit s)
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
    breakup (Doublequoted ps) = dqBreakup ps
    breakup (IFS _) = [Separator Explicit]
    breakup p = [FieldPart p]

    dqBreakup ps = let (qs,rs) = break isIFS ps in
        (FieldPart $ Doublequoted qs) : case rs of
            [] -> []
            (_:rs') -> Separator Explicit : dqBreakup rs'

    isIFS (IFS _) = True
    isIFS _ = False

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


byPathParts :: (Monad m, Functor m) => (Word -> m Word) -> Word -> m Word
byPathParts f w = unparts <$> mapM f (expandParts (byparts []) w)
  where
    byparts [] [] = []
    byparts acc [] = reverse acc : []
    byparts acc (Bare s:ws) = case break (== ':') s of
        (r,(':':q)) -> reverse (Bare r:acc) : byparts [] (Bare q:ws)
        _ -> byparts (Bare s:acc) ws
    byparts acc (x:ws) = byparts (x:acc) ws

    unparts = Word (location w) . intercalate [Bare ":"] . map parts


-- | Expands content that has been parsed for parameter, command, and
-- arithmetic expansions. Note that the input type, 'Parts' is somewhat more
-- general, and other constructs, if present, be expanded.
contentExpansion :: (PosixLike m) => String -> ShellExec m String
contentExpansion s = evalExpansion $
    concatMap partText . concat <$> mapM (basicExpansion True) (parseContent s)
