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

module Plush.Run.BuiltIns (
    special,
    direct,
    builtin,
    pseudoExecs
    )
where

import qualified Data.HashMap.Strict as M

import Plush.Run.BuiltIns.Evaluation
import Plush.Run.BuiltIns.FileSystem
import Plush.Run.BuiltIns.Grep
import Plush.Run.BuiltIns.Plush
import Plush.Run.BuiltIns.ShellState
import Plush.Run.BuiltIns.Text
import Plush.Run.BuiltIns.Trivial
import Plush.Run.BuiltIns.Utilities
import Plush.Run.BuiltIns.WorkingDirectory
import Plush.Run.Posix


-- | Special Built-In Utilities (§2.14)
-- These found without PATH search, and execute directly within the shell.
-- In particular, variable assignments made with these commands affect the
-- shell.
special :: (PosixLike m) => String -> Maybe (ShellUtility m)
special = flip M.lookup $ M.fromList $ map (fixup unSpecial)
    [ ("complete", complete)
    , ("context", context)
    , (":", colon)
    , ("set", set)
    , ("export", export)
    , ("readonly", readonly)
    , ("unset", unset)
    , ("shift", shift)
    , (".", dot)
    , ("eval", eval)
    , ("plush-version", plushVersion)
    ]
    -- eventually will include:
    -- break, continue, exec, exit
    -- return, times, trap
    -- plush extensions:
    -- complete, context

-- | Direct Built-In Utilities (§2.9.1)
-- These are executed without PATH search. These commands may have side-effects
-- that affect the shell environment (for example cd changes the shells cwd),
-- but are invoked as if they were external commands. In particular, variable
-- assignments only affect the environment variables available to these commands
-- and not the shell's variables.
direct :: (PosixLike m) => String -> Maybe (ShellUtility m)
direct = flip M.lookup $ M.fromList $ map (fixup unDirect)
        [ ("cd", cd)
        , ("env", env)
        , ("false", false)
        , ("true", true)
        , ("alias", alias)
        , ("unalias", unalias)
        ]
    -- eventually will also include:
    -- bg, command, fc, fg, getopts, jobs, kill,
    -- newgrp, pwd, read, umask, wait

-- | Regular Built-In Utilities (§2.9.1)
-- If PATH search succeeds in finding an executable for these, then the built-in
-- version may be executed instead. However, it is run in an environment
-- equivalent to one the executable would have run in. These built-ins can't
-- affect the shell state.
builtin :: (PosixLike m) => String -> Maybe (ShellUtility m)
builtin = flip M.lookup $ M.fromList $ map (fixup unBuiltIn)
    alwaysBuiltin

-- | Utilities that should act as executables in the TestExec monad.
-- These will be found by path search, and then "executed". TestExec arranges
-- that when the corresponding files are exectued, these run.
pseudoExecs :: (PosixLike m) => M.HashMap String (RegularUtility m)
pseudoExecs = M.fromList $ map (fixup unUtility) $
    alwaysBuiltin ++ otherBuiltins


-- | Utilities that are always built into the shell. They are also available
-- as exectuables (in TestExec), since they are subject to path search and
-- won't be executed (even as builtin) unless they are found and exectuable.
--
-- Only put utilities on this list that fully implement the Posix behavior,
-- as they will override the executables on the user's system.
alwaysBuiltin :: (PosixLike m) => [(String, BuiltInUtility m)]
alwaysBuiltin =
    [ ("echo", echo)
    , ("recho", recho)
    ]

-- | Utilities that are only available as executables from the TestExec monad.
otherBuiltins :: (PosixLike m) => [(String, BuiltInUtility m)]
otherBuiltins =
    [ ("cat", cat)
--  , ("egrep", egrep)  -- TODO: enable once regular expressions are implemented
    , ("fgrep", fgrep)
--  , ("grep", grep)    -- TODO: enable once regular expressions are implemented
    , ("mkdir", mkdir)
    , ("rm", rm)
    , ("touch", touch)
    , ("tr", tr)
    ]

fixup :: (a -> b -> c) -> (a, b) -> (a, c)
fixup f (a, b) = (a, f a b)
