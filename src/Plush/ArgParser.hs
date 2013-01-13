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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | A generalized command line argument parser with these features:

    * Conforms to POSIX §12 Utilty Conventions
    * Support for + options (as used by set and sh utilities)
    * Support for long option names.
    * Flexible output so the same system can be used for both running,
      annotation, and command completion.
    * Human readable strings for help are not embedded in the defintions,
      making internationalization easier.
-}

module Plush.ArgParser (

    -- * Option Specification
    OptionSpec(..),
    ArgSpec(..),

    -- * Option Combinators
    -- $combinators
    optOn, optFirst, optSecond,

    -- * Processing
    processArgs,

    ArgInfo(..),
    OptsAndArgs(..),
    ArgAnnotations(..),

    ) where


import Control.Arrow (first, second)
import Control.Monad (liftM2)
import Data.Maybe (listToMaybe)
import Data.Monoid
import qualified Data.Text as T

import Plush.Run.Types
import Plush.Types.CommandSummary



-- | An option specification over the option state of type @/a/@. Use the
-- functions 'flag', 'toggle', etc... to create specifications, which are
-- then passed to 'stdSyntax' to generate a 'Utility'.
data OptionSpec a = OptionSpec [Char] (ArgSpec a)

data ArgSpec a = NoArg (a -> a)
               | ReqArg (String -> a -> a)
               | OptArg (Maybe String -> a -> a)


-- $combinators
-- These combintors transform an operation on one type of a option state into
-- another. For example, a utility with some flags (whose option state is a list
-- of flags), and an arg option (whose option state is the string value), will
-- want to have an option state of a pair:
--
-- >   example = BuiltInUtility $ stdSyntax options ("a", "") go
-- >     where
-- >       options =
-- >           [ optFirst $ flag 'x'           -- extra special
-- >           , optFirst $ toggle 'a' "ab"    -- choose apples
-- >           , optFirst $ toggle 'b' "ab"    -- choose bananas
-- >           , optSecond $ optArg 'q' "ADJ"  -- apply adjective
-- >           ]
-- >       go (flags, adj) _args = do
-- >           putStrLn $ intercalate " " [extra, adj, fruit]
-- >           success
-- >         where
-- >           extra = if 'x' `elem` flags then "extra" else ""
-- >           fruit | 'a' `elem` flags = "apples"
-- >                 | 'b' `elem` flags = "bananas"
-- >                 | otherwise        = "generic fruit"

-- | Redeclare an option as the first value in the options argument.
optFirst :: OptionSpec a -> OptionSpec (a,b)
optFirst = optOn first

-- | Redeclare an option as the second value in the options argument.
optSecond :: OptionSpec b -> OptionSpec (a,b)
optSecond = optOn second

-- | Redclare an option via a function that applies a modifier function to part
-- of the option state.
optOn :: ((b -> b) -> (c -> c)) -> OptionSpec b -> OptionSpec c
optOn editor (OptionSpec fs ad) = OptionSpec fs (arg ad)
  where
    arg (NoArg f) = NoArg $ editor f
    arg (ReqArg f) = ReqArg $ editor . f
    arg (OptArg f) = OptArg $ editor . f



-- | A type that can collect information about arguments. The type takes one
-- type argument @/a/@, which is the same type used in 'OptDesc'.
--
-- When applied to @/a/@, an 'ArgInfo' type should also be a 'Monoid'. That
-- constraint can't be enforced here, but is in the functions that use them.
class ArgInfo ai where
    option :: (a -> a) -> Maybe T.Text -> ai a
    optionArg :: String -> ai a
    optionError :: String -> ai a
    separator :: ai a
    argument :: String -> ai a


newtype OptsAndArgs a = OA (Either String ((a -> a), Args))

instance Monoid (OptsAndArgs a) where
    mempty = OA $ Right (id, [])
    mappend (OA x) (OA y) = OA $ liftM2 (\(f,as) (g,bs) -> (g.f, as++bs)) x y

instance ArgInfo OptsAndArgs where
    option f _ = OA $ Right (f, [])
    optionArg _ = mempty
    optionError = OA . Left
    separator = mempty
    argument a = OA $ Right (id, [a])


newtype ArgAnnotations a = AS { argAnnos :: [Annotation] }
    deriving (Monoid)

instance ArgInfo ArgAnnotations where
    option _ (Just d) = AS [OptionAnno d]
    option _ _ = mempty
    optionArg _ = mempty
    optionError s = AS [OptionAnno $ T.pack s]
    separator = mempty
    argument _ = mempty


{-
instance ArgInfo Completion where
    allArgs = map $ const path
    optError = chooseOpts
    option _o _f = chooseOpts
    separator = ChooseComp ["--"]
-}

-- | The workhorse of this module. Processes a command line, calling the
-- methods of 'ArgInfo' as it parses. Returns an array of 'ArgInfo',
-- corresponding one for one with the 'Args' array passed in.
processArgs :: (ArgInfo ai, Monoid (ai a)) =>
    CommandSummary -> [OptionSpec a] -> Args -> [ai a]
processArgs summ opts args = lookForOpts args
  where
    lookForOpts [] = []
    lookForOpts (a:as) = case a of
        "--"                -> separator : remainingArgs as
        ('-':os@(_:_))      -> shortOpts mempty os as
        _                   -> remainingArgs (a:as)

    shortOpts i [] as = i : lookForOpts as
    shortOpts i (o:os) as = case findShort o of
        Nothing -> shortOpts (i `mappend` unknown) os as
        Just (OptionSpec _ arg) -> case arg of

            NoArg f -> shortOpts (i `mappend` option f desc) os as

            ReqArg f -> case (os, as) of
                ([], [])      -> done     missing                           []
                ([], (a:as')) -> doneArg  (option (f a) desc) (optionArg a) as'
                (a, as')      -> doneArg' (option (f a) desc) (optionArg a) as'

            OptArg f -> case os of
                [] -> done     (option (f Nothing) desc)                  as
                _  -> doneArg' (option (f $ Just os) desc) (optionArg os) as
      where
        done     oi    rs = i `mappend` oi              : lookForOpts rs
        doneArg  oi ai rs = i `mappend` oi : ai         : lookForOpts rs
        doneArg' oi ai rs = i `mappend` oi `mappend` ai : lookForOpts rs

        o' = ['-',o]
        desc = findOptionDescription o' summ
        unknown = optionError ("unknown option: " ++ o')
        missing = optionError ("missing option argument for: " ++ o')

    remainingArgs = map argument

    findShort o = listToMaybe $ filter (\(OptionSpec s _) -> o `elem` s) opts


