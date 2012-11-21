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

{-# Language MultiParamTypeClasses, FlexibleInstances #-}

module Plush.Parser.Aliases (
    -- * Aliases
    Aliases,
    aliasNameChar,

    DealiasingStream,
    dealiasStream,
    dealiasRemainder,

    aliasSubstitution,
    )
where

import Control.Arrow (second)
import Control.Monad
import Data.Char (isAlpha, isDigit)
import qualified Data.HashMap.Strict as M
import Text.Parsec


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
data DealiasingStream = DAS Aliases [Level]

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
dealiasStream aliases s = DAS aliases [(False, "", s)]

-- | Return the remainder of the unstreamed input.
dealiasRemainder :: DealiasingStream -> String
dealiasRemainder (DAS _ stk) = concatMap (\(_, _, cs)->cs) stk

-- | Workhorse of the stream: Return the next character if any.
dasUncons (DAS aliases stk0) = second (DAS aliases) `fmap` go stk0
  where
    go [] = Nothing                             -- empty stack
    go ((_,      _,     [])     :stk) = go stk  -- pop completed level
    go ((False, aname, (c  :cs)):stk) = Just (c, (False, aname, cs):stk)
        -- if not doing alias substitution, just return next character
    go ((True,  aname, (' ':cs)):stk) = Just (' ', (True, aname, cs):stk)
        -- if doing alias substitution, slew blanks out
    go ((True,  aname,  cs)     :stk) =
        -- if doing alias substitution, see if next word is an alias
        let (w,cs') = span aliasNameChar $ cs
            mr = if w `elem` (aname:map (\(_,a,_)->a) stk)
                    then Nothing             -- already in progress
                    else M.lookup w aliases  -- see if it is an alias
        in case mr of
            Nothing -> go ((False, aname, cs):stk)  -- treat as normal
            Just replace -> go ((True, w, replace)  -- stack replacement
                               :(endsInBlank replace, aname, cs')  -- remainder
                               :stk)

    endsInBlank [] = False
    endsInBlank [c] = c == ' '
    endsInBlank (_:cs) = endsInBlank cs


-- | Enable alias substition for the next word
aliasSubstitution :: Parsec DealiasingStream u ()
aliasSubstitution = void $
    updateParserState $ \s -> s { stateInput = mark $ stateInput s}
  where
    mark (DAS as ((_, aname, cs):stk)) = DAS as ((True, aname, cs):stk)
    mark d = d

