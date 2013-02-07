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

module Plush.Parser.Base (
    -- * Parsing type
    ShellParser,
    shellParse,

    nextHereDoc, parseQueuedHereDocs,

    -- * General utilities
    -- ** Parsing two things
    (<&>),

    -- ** Parsing Monoids
    -- $utilmonoid
    (<++>),
    moptional
    )
where

import Control.Monad
import Data.Monoid
import Text.Parsec

import Plush.Parser.Aliases
import Plush.Types


-- | The parser type used for parsing shell commands
type ShellParser = Parsec DealiasingStream ShellParseState

-- | This is like 'Parsec.parse', but for for 'ShellParser's.
-- It takes care of the knotted state.
shellParse :: (ShellParser a) -> String -> DealiasingStream
    -> Either ParseError a
shellParse act fp input = fst `fmap` parseResult
  where
    knottedState = baseShellParseState { spsKnottedHereDocs = extractHereDocs }
    extractHereDocs = either (const []) (spsHereDocs . snd) parseResult
    parseResult = runParser (act <&> getState) knottedState fp input



-- | State held during the parse. This type is opaque to clients of this
-- module. Try to keep this to a minimum.
data ShellParseState =
    SPS { spsQueuedHereDocParsers :: [ShellParser HereDoc]
            -- ^ parsers deferred until later
        , spsHereDocs :: [HereDoc]
            -- ^ here docs parsed
        , spsKnottedHereDocs :: [HereDoc]
            -- ^ all the here docs, from the future! (see tying-the-knot)
        }

baseShellParseState :: ShellParseState
baseShellParseState =
    SPS { spsQueuedHereDocParsers = []
        , spsHereDocs = []
        , spsKnottedHereDocs = []
        }

-- | Supply a parser for a here-doc, and return that here-doc, even though
-- the parser is not run now, but at some point in the future.
--
-- Calling this function implies a promise to later call 'parseQueuedHereDocs'
-- which will call the parser. If the promise is not fulfilled, 'HereDocMissing'
-- will result.
--
-- Care must be taken to not inspect the here-doc returned until after the call
-- to shellParse completes, because this future reference is achieved via the
--  "tying the knot" technique.
nextHereDoc :: ShellParser HereDoc -> ShellParser HereDoc
nextHereDoc p = do
    s <- getState
    putState s { spsQueuedHereDocParsers = spsQueuedHereDocParsers s ++ [p]
               , spsKnottedHereDocs = drop 1 $ spsKnottedHereDocs s
               }
    return $ extractHereDoc (spsKnottedHereDocs s)
  where
    extractHereDoc [] = HereDocMissing
    extractHereDoc (hd:_) = hd

-- | Parse any queued here docs.
parseQueuedHereDocs :: ShellParser ()
parseQueuedHereDocs = do
    s <- getState
    putState s { spsQueuedHereDocParsers = [] }
    sequence_ $ map stashHereDoc $ spsQueuedHereDocParsers s
  where
    stashHereDoc p = do
        hdoc <- p
        modifyState $ \s -> s { spsHereDocs = spsHereDocs s ++ [hdoc] }



--
-- General Utilities
--

-- | Parse two things, returning both results in a tuple
(<&>) :: Monad m => m a -> m b -> m (a,b)
(<&>) = liftM2 (,)
infix 5 <&>


-- $utilmonoid
-- Parsing Monoid results can be made simpler with these combinators

-- | Parse two things, returning the mappend of the results
(<++>) :: (Monad m, Monoid a) => m a -> m a -> m a
(<++>) = liftM2 mappend
infix 5 <++>

-- | Parse something optionally, returning mempty if absent
moptional :: (Monad m, Monoid a) => ParsecT s u m a -> ParsecT s u m a
moptional a = a <|> return mempty
