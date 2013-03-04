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

module Plush.Run.BuiltIns.Syntax (

    -- * Standard Utiilty Syntax
    stdSyntax,
    perArg,

    -- * Option Specification
    flag, flagAlt, toggle, argOpt,

    ) where


import Control.Monad.Exception (catchAll)
import Data.List ((\\))
import Data.Monoid

import Plush.ArgParser
import Plush.Run.Posix
import Plush.Run.Posix.Return
import Plush.Run.Posix.Utilities
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
stdSyntax :: (PosixLike m, Returnable r) =>
    [OptionSpec a]          -- ^ options
    -> a                    -- ^ the initial option state
    -> (a -> Args -> m r)   -- ^ utility function
    -> CommandSummary       -- ^ summary information for error help messages
    -> Utility m r
stdSyntax options opt0 action summary = Utility exec anno
  where
    exec cmdLineArgs = case mconcat $ processArgs options cmdLineArgs of
        OA (Right (optF, args)) -> reportError $ action (optF opt0) args
        OA (Left errs) -> do
            errStr errs
            errStrLn $ ciSynopsis summary
            errStr $ formatOptions summary
            failure

    anno cmdLineArgs = return $
        map (argAnnotations summary) $ processArgs options cmdLineArgs

-- | When a utility simply applies to each of the arguments in turn, this
-- transforms a function of option state and single arg into the function
-- of option state and all args needed by 'stdUtility'.
perArg :: (PosixLike m) =>
    (a -> String -> m ExitCode)
    -> a -> [String] -> m ExitCode
perArg cmd opts args = mapM (reportError . cmd opts) args >>= return . maximum

-- | Catch any errors, report them, and return an error exit code.
reportError :: (PosixLike m, Returnable r) => m r -> m r
reportError act = act `catchAll`  (exitMsg 1 . show)



-- | Declare a flag option. The option state is a list of flag characters,
-- last flag from the command line, first. If a flag is repeated, it will
-- only be included in the state once.
flag :: Char -> OptionSpec String
flag f = OptionSpec [f] [] (NoArg (\fs -> f : (fs \\ [f])))

-- | Declare a flag option, with possible alternatives. The first flag
-- listed will be used as the canonical flag added to the option state.
flagAlt :: String -> OptionSpec String
flagAlt fa@(f0:_) = OptionSpec fa [] (NoArg (\fs -> f0 : (fs \\ fa)))
flagAlt [] = error "Plush.Run.Builtins.Syntax.flagAlt: no flags supplied"

-- | Declare a flag that is among an exclusive set of flags. Only one of
-- the flags will appear in the final option state. In the initial option
-- state supplied to 'stdUtility', either include the default flag as, or
-- include none if you want to detect if the user didn't specify a choice.
toggle :: Char  -- ^ the flag character
    -> String   -- ^ the mutually exclusive set of flags
    -> OptionSpec String
toggle f excl = OptionSpec [f] [] (NoArg (\fs -> f : (fs \\ excl)))

-- | Declare a string option. The option state is the value of this option.
-- Supply the default as the initial option state to 'stdUtility'.
argOpt :: Char  -- ^ the flag character
    -> OptionSpec String
argOpt f = OptionSpec [f] [] (ReqArg const)
