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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Plush.Run.BuiltIns.Syntax (

    -- * Standard Utiilty Syntax
    stdSyntax,

    -- * Option Specification
    OptionSpec,
    flag, flagAlt, toggle, argOpt,

    -- * Option Combinators
    -- $combinators
    optFirst, optSecond,

    -- * Misc.
    perArg,

    ) where


import Control.Arrow (first, second)
import Control.Monad (liftM2)
import Control.Monad.Exception (catchAll)
import Data.List ((\\))
import Data.Maybe (listToMaybe)
import Data.Monoid
import qualified Data.Text as T

import Plush.Run.Posix
import Plush.Run.Types
import Plush.Types.CommandSummary


-- | Create a 'Utility' that can parse options on execution, as well as
-- provide annotations and completions of candidate command lines.
--
-- Option parsing proceeds according to the \"Utility Conventions\" in §12.
-- This is essentially the common conventions for single letter options, and
-- using "--" to end argument processing.
--
-- The supplied utility function takes the resulting option state and the
-- arguments to produce an 'ExitCode'.
--
-- The 'CommandSummary' argument is last because it is supplied by the
-- machinery in "Plush.Run.BuiltIns", since that is where a utility's
-- implementation is associated with a name, which is used there to find
-- the summary info. Hence, typical usage is:
--
-- @
--   example = BuiltInUtility $ stdSyntax options initialFlags exec
--     where
--       exec flags args = ...
--       ...
-- @
stdSyntax :: (PosixLike m) =>
    [OptionSpec a]                  -- ^ options
    -> a                            -- ^ the initial option state
    -> (a -> Args -> m ExitCode)    -- ^ utility function
    -> CommandSummary   -- ^ summary information for error help messages
    -> Utility m
stdSyntax options opt0 action summary = Utility exec anno
  where
    exec cmdLineArgs = case mconcat $ processArgs summary options cmdLineArgs of
        OA (Right (optF, args)) -> reportError $ action (optF opt0) args
        OA (Left errs) -> do
            errStr errs
            errStrLn $ ciSynopsis summary
            errStr $ formatOptions summary
            failure

    anno cmdLineArgs = return $
        map argAnnos $ processArgs summary options cmdLineArgs

-- | An option specification over the option state of type @/a/@. Use the
-- functions 'flag', 'toggle', etc... to create specifications, which are
-- then passed to 'stdSyntax' to generate a 'Utility'.
data OptionSpec a = OptionSpec [Char] (ArgSpec a)

data ArgSpec a = NoArg (a -> a)
               | ReqArg (String -> a -> a)
               | OptArg (Maybe String -> a -> a)

-- | Declare a flag option. The option state is a list of flag characters,
-- last flag from the command line, first. If a flag is repeated, it will
-- only be included in the state once.
flag :: Char -> OptionSpec String
flag f = OptionSpec [f] (NoArg (\fs -> f : (fs \\ [f])))

-- | Declare a flag option, with possible alternatives. The first flag
-- listed will be used as the canonical flag added to the option state.
flagAlt :: String -> OptionSpec String
flagAlt fa@(f0:_) = OptionSpec fa (NoArg (\fs -> f0 : (fs \\ fa)))
flagAlt [] = error "Plush.Run.Builtins.Utilities.flagAlt: no flags supplied"

-- | Declare a flag that is among an exclusive set of flags. Only one of
-- the flags will appear in the final option state. In the initial option
-- state supplied to 'stdUtility', either include the default flag as, or
-- include none if you want to detect if the user didn't specify a choice.
toggle :: Char  -- ^ the flag character
    -> String   -- ^ the mutually exclusive set of flags
    -> OptionSpec String
toggle f excl = OptionSpec [f] (NoArg (\fs -> f : (fs \\ excl)))

-- | Desclare a string option. The option state is the value of this option.
-- Supply the default as the initial option state to 'stdUtility'.
argOpt :: Char  -- ^ the flag character
    -> OptionSpec String
argOpt f = OptionSpec [f] (ReqArg const)

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


-- | When a utility simply applies to each of the arguments in turn, this
-- transforms a function of option state and single arg into the function
-- of option state and all args needed by 'stdUtility'.
perArg :: (PosixLike m) =>
    (a -> String -> m ExitCode)
    -> a -> [String] -> m ExitCode
perArg cmd opts args = mapM (reportError . cmd opts) args >>= return . maximum

-- | Catch any errors, report them, and return an error exit code.
reportError :: (PosixLike m) => m ExitCode -> m ExitCode
reportError act = act `catchAll`  (exitMsg 1 . show)


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


