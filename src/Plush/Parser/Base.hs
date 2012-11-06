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

module Plush.Parser.Base (
    -- * Parsing type
    ShellParser,

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


-- | The parser type used for parsing shell commands
type ShellParser = Parsec String ()


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
