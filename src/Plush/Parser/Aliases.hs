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

{-# Language MultiParamTypeClasses, FlexibleInstances #-}

module Plush.Parser.Aliases (
    -- * Aliases
    Aliases,
    aliasNameChar,

    DealiasingStream,
    dealiasStream,
    dealiasRemainder,

    aliasSubstitution,
    originalSourceColumn
    )
where

import Control.Arrow (first)
import Control.Monad
import Data.Char (isAlpha, isDigit)
import qualified Data.HashMap.Strict as M
import Text.Parsec

import Plush.Utilities


type Aliases = M.HashMap String String

-- | Legal characters for an alias name are alpha-numerics and _, !, %, @, and
-- comma.
aliasNameChar :: Char -> Bool
aliasNameChar '_' = True
aliasNameChar c | isDigit c = True
aliasNameChar c | isAlpha c = True
aliasNameChar '!' = True
aliasNameChar '%' = True
aliasNameChar ',' = True
aliasNameChar '@' = True
aliasNameChar _ = False


-- | A stream of input that can apply aliases as it is streamed. The input is
-- kept as a stack of string in progress. Each is an alias explansion in
-- progress, save the last, which is just the original string.
--
-- The `Int` is the column (statring at 1), of the next character that will be
-- `uncons`'d, in terms of the original string.
data DealiasingStream = DAS Aliases Int [Level]

instance (Monad m) => Stream DealiasingStream m Char where
    uncons = return . dasUncons

-- | Each level has three components:
--
-- 1. a flag: if alias substitution should be attempted on the first word
--
-- 2. the name of the alias the string came from (and hence is "in progress")
--
-- 3. the remainder of the string.
type Level = (Bool, String, String)

-- | Construct a stream given the aliases to apply, and the original input.
dealiasStream :: Aliases -> String -> DealiasingStream
dealiasStream aliases s = DAS aliases 1 [(False, "", s)]

-- | Return the remainder of the unstreamed input.
dealiasRemainder :: DealiasingStream -> String
dealiasRemainder (DAS _ _ stk) = concatMap (\(_, _, cs)->cs) stk


-- Utilities

-- | Return next character and substitution flag, if any,
nextChar :: DealiasingStream -> Maybe (Bool, Char, DealiasingStream)
nextChar (DAS _  _   []) = Nothing
nextChar (DAS as col ((f, _, ""):stk)) =
    nextChar $ mergeSubst f (DAS as col stk)
nextChar (DAS as col ((f, an, c:cs):stk)) =
    Just (f, c, DAS as (col + inc) ((f, an, cs):stk))
  where inc = if null stk then 1 else 0

-- | Return the next span of valid alias name characters.
nextNameSpan :: DealiasingStream -> (String, DealiasingStream)
nextNameSpan d = case nextChar d of
    Just (_, c, d') | aliasNameChar c -> first (c:) $ nextNameSpan d'
    _ -> ([], d)

-- | Set the substition flag for the current level.
modifySubst :: (Bool -> Bool) -> DealiasingStream -> DealiasingStream
modifySubst f (DAS as col ((fl, an, cs):stk)) = DAS as col ((f fl, an, cs):stk)
modifySubst _ d = d

setSubst, mergeSubst :: Bool -> DealiasingStream -> DealiasingStream
setSubst = modifySubst . const
mergeSubst = modifySubst . (||)

-- | Push an alais expansion on the level stack.
stack :: Level -> DealiasingStream -> DealiasingStream
stack lvl (DAS as col stk) = DAS as col (lvl:stk)

aliasesInProgress :: DealiasingStream -> [String]
aliasesInProgress (DAS _ _ stk) = map (\(_, an, _) -> an) stk

endsInBlank :: String -> Bool
endsInBlank [] = False
endsInBlank [c] = isBlank c
endsInBlank (_:cs) = endsInBlank cs

-- | Workhorse of the stream: Return the next character if any.
dasUncons :: DealiasingStream -> Maybe (Char, DealiasingStream)
dasUncons d@(DAS aliases _ _) = nextChar d >>= go
  where
    go (False, c, d') = Just (c, d')     -- not doing alias substitution (AS)
    go (True, c, d')
        | isBlank c   = dasUncons d'     -- drop leading blanks, when doing AS
        | otherwise   =                  -- doing AS, so look at next name
        let (w, dw') = nextNameSpan d
            mr = if null w || w `elem` aliasesInProgress d
                then Nothing             -- already in progress
                else M.lookup w aliases  -- see if it is an alias
        in case mr of
            Nothing -> Just (c, setSubst False d')      -- turn off AS
            Just replace -> dasUncons
                            $ stack (True, w, replace)  -- stack the replacement
                            $ setSubst (endsInBlank replace) dw'
                                         -- blank at the end retriggers AS

-- | Enable alias substition for the next word
aliasSubstitution :: Parsec DealiasingStream u ()
aliasSubstitution = void $
    updateParserState $ \s -> s { stateInput = setSubst True $ stateInput s}

originalSourceColumn :: Parsec DealiasingStream u Int
originalSourceColumn = (dasColumn . stateInput) `fmap` getParserState
  where
    dasColumn (DAS _ c _) = c
