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

{-# Language TupleSections #-}

module Plush.Run.Command (
    FoundCommand(..),
    UtilityRunner,
    FunctionRunner,
    CommandAction(..),
    CommandAnnotate,
    commandSearch,
    ) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Exception (finally)
import System.FilePath

import {-# SOURCE #-} Plush.Run.BuiltIns -- see BuiltIns.hs-boot
import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Run.Types
import Plush.Types


-- | A function to run a utility. The `Bindings` are the variable assignments
-- from the command line, since how they are interpreted depends on the type
-- of utility being invoked.
type UtilityRunner m = Bindings -> Args -> ShellExec m ExitCode

-- | To run a shell function, a function to execute the body of the function
-- must be passed in.
type FunctionRunner m =
    (FunctionBody -> ShellExec m ExitCode) -> UtilityRunner m

-- | The found action for a utility includes the function (`UtilityRunner`) for
-- how to execute it. When the utility turns out to be a function, this code
-- doesn't have enough information to run, so returns a function that can.
data CommandAction m = UtilityAction (UtilityRunner m)
                    | FunctionAction (FunctionRunner m)

-- | A function to annotate a command
type CommandAnnotate m = Args -> ShellExec m [[Annotation]]

-- | Use the command name to find a command. Returns information about where
-- the command was found, and a @'ShellUtility' m@ to run it. Implements the
-- search algorithm in §2.9.1 "Command Search and Execution".
commandSearch :: (PosixLike m) => String
    -> ShellExec m (FoundCommand, CommandAction m, CommandAnnotate m)
commandSearch cmd
    | '/' `elem` cmd = external cmd
    | otherwise  =
        -- step 1.a.: special built-ins
        search special setShellVars SpecialCommand $

        -- step 1.b.: functions
        searchFun $

        -- step 1.c.: direct utilities
        search direct withEnvVars DirectCommand $

        -- step 1.d.: search on PATH
        findOnPath
  where
    findOnPath = do
        getVarDefault "PATH" "" >>= go . map (</> cmd) . splitSearchPath
      where
        go (fp:fps) = do
            b <- doesFileExist fp -- TODO: check if it can be executed
            if b
                then -- step 1.d.i.a.: regular built-ins
                     search builtin withEnvVars (BuiltInCommand fp) $
                     -- step 1.d.i.b.: exec from file system
                     external fp
                else go fps
        go [] = search builtin withEnvVars (BuiltInCommand "/???") $
                -- step 1.d.ii.: not found on PATH
                unknown
            -- TODO: technically shouldn't run the builtins if not found in
            -- during path search. But for now, since the test environemnt
            -- doesn't have them, this will fail to run anything!

    search lkup processBindings fc alt =
        maybe alt (\util ->
            return (fc,
                    UtilityAction (processBindings $ utilExecute util),
                    utilAnnotate util))
            $ lkup cmd

    searchFun alt = do
        maybeFun <- getFun cmd
        maybe alt (\fb ->
            return (FunctionCall,
                    FunctionAction $ functionExec fb,
                    emptyAnnotate))
            $ maybeFun

    external fp = return (ExecutableCommand fp,
                          UtilityAction $ externalExec fp,
                          emptyAnnotate)
    externalExec fp = withEnvVars $ \args -> do
        env <- getEnv
        execProcess env fp args

    functionExec fb ef = setShellVars (\args -> withFunContext args $ ef fb)
        -- §2.9.5 states that when executed, functions have the assignment
        -- properties of special built-ins.

    unknown = return (UnknownCommand, UtilityAction unknownExec, emptyAnnotate)
    unknownExec _ _ = exitMsg 127 ("Unknown command: " ++ cmd)

    -- TODO: unsure if emptyAnnotate is correct here, or perhaps defaultAnnotate

setShellVars :: (PosixLike m) =>
                (Args -> ShellExec m ExitCode) ->
                Bindings -> Args -> ShellExec m ExitCode
setShellVars exec bindings args =
    untilFailureM setShellVar bindings `andThenM` exec args
  where
    setShellVar (name, val) =
        setVarEntry name (VarShellOnly, VarReadWrite, Just val)

withEnvVars :: (PosixLike m) =>
               (Args -> ShellExec m ExitCode) ->
               Bindings -> Args -> ShellExec m ExitCode
withEnvVars exec bindings args = do
    prev <- currentVarEntries bindings
    finally (setEnvVars bindings `andThenM` exec args)
        (forM_ prev restoreVarEntries)

currentVarEntries :: (PosixLike m) =>
    Bindings -> ShellExec m [(String, Maybe VarEntry)]
currentVarEntries as = mapM (\(name, _) -> (name,) <$> getVarEntry name) as

setEnvVars :: (PosixLike m) => Bindings -> ShellExec m ExitCode
setEnvVars bindings = untilFailureM setEnvVar bindings
  where
    setEnvVar (name, val) =
        setVarEntry name (VarExported, VarReadWrite, Just val)

restoreVarEntries (name, Just val) = void $ setVarEntry name val
restoreVarEntries (name, Nothing) = void $ unsetVarEntry name
