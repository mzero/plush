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

module Plush.Run.Execute (
    shellExec,
    )
where

import Control.Monad
import Data.Functor

import Plush.Run.Command
import Plush.Run.Expansion
import Plush.Run.Posix
import Plush.Run.Redirection
import Plush.Run.ShellExec
import Plush.Run.Types
import Plush.Types


shellExec :: (PosixLike m) => CommandList -> ShellExec m ()
shellExec cl =  execCommandList cl
  where
    execCommandList = mapM_ execCommandItem

    execCommandItem (ao, Sequential) = execAndOr ao
    execCommandItem (_, Background) = notSupported "Background execution"

    execAndOr ao = foldM execAndOrItem 0 ao

    execAndOrItem 0 (AndThen, (s, p))         = execPipeSense s p
    execAndOrItem e (OrThen, (s, p)) | e /= 0 = execPipeSense s p
    execAndOrItem e _ = return e

    execPipeSense s p = do
        e <- sense s `fmap` execPipe p
        setLastExitCode e
        return e

    sense Normal   e = e
    sense Inverted 0 = 1
    sense Inverted _ = 0

    execPipe [] = exitMsg 120 "Emtpy pipeline"
    execPipe [c] = execCommand c
    execPipe cs = pipeline $ map execCommand cs

    execCommand (Command [] _ (_:_)) = notSupported "Bare redirection"
    execCommand (Command [] as []) = mapM_ processAssignment as >> success
    execCommand (Command _ (_:_) _) = notSupported "Assignment to Environment"
    execCommand (Command ws [] rs) =
        expandAndSplit ws >>= withRedirection rs . execFields

    execFields (cmd:args) = findCmd cmd >>= ($ args) . utilExecute
    execFields [] = exitMsg 122 "Empty command"

    findCmd cmd = snd `fmap` commandSearch cmd

processAssignment :: (PosixLike m) => Assignment -> ShellExec m ()
processAssignment (Assignment name w) = do
    v <- quoteRemoval <$> byPathParts wordExpansionActive w
    setVarEntry name (VarShellOnly, VarReadWrite, v)
    -- TODO: doesn't respect the modifiers on the var, if any

