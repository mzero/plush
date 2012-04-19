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

module Plush.Run.Command (
    FoundCommand(..),
    commandSearch,
    ) where

import System.FilePath

import {-# SOURCE #-} Plush.Run.BuiltIns -- see BuiltIns.hs-boot
import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Run.Types



-- | Use the command name to find a command. Returns information about where
-- the command was found, and a @'ShellUtility' m@ to run it. Implements the
-- search algorithm in §2.9.1.
commandSearch :: (PosixLike m) =>
    String -> ShellExec m (FoundCommand, ShellUtility m)
commandSearch cmd
    | '/' `elem` cmd = external cmd
    | otherwise  =
        search special SpecialCommand $ search direct DirectCommand $ findOnPath
  where
    findOnPath = do
        getVarDefault "PATH" "" >>= go . map (</> cmd) . splitSearchPath
      where
        go (fp:fps) = do
            b <- doesFileExist fp -- TODO: check if it can be executed
            if b
                then search builtin (BuiltInCommand fp) $ external fp
                else go fps
        go [] = search builtin (BuiltInCommand "/???") $ unknown
            -- TODO: technically shouldn't run the builtins if not found in
            -- during path search. But for now, since the test environemnt
            -- doesn't have them, this will fail to run anything!

    search lkup fc alt = maybe alt (\util -> return (fc, util)) $ lkup cmd

    external fp = return (ExecutableCommand fp,
                    Utility (rawSystem fp) emptyAnnotate)
        -- TODO: ensure exported environment is correct

    unknown = return (UnknownCommand, Utility unknownExec emptyAnnotate)
    unknownExec _ = exitMsg 127 ("Unknown command: " ++ cmd)

    -- TODO: unsure if emptyAnnotate is correct here, or perhaps defaultAnnotate
