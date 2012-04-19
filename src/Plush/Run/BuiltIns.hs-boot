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

{-
    This is the "boot" file for Plush.Run.Builtins. It is used to break a module
    include cycle for GHC.

    The problem is that some builtin commands, in particular 'complete', need
    to be able to use the command look up machinery to be able to gather
    information about commands. However, that very machinery uses Builtins
    to be able to locate the builtin commands, which in turn depends on the
    module exporting the 'complete' command.
-}

module Plush.Run.BuiltIns (
    special,
    direct,
    builtin,
    )
where

import Plush.Run.Posix
import Plush.Run.ShellExec

special :: (PosixLike m) => String -> Maybe (ShellUtility m)
direct :: (PosixLike m) => String -> Maybe (ShellUtility m)
builtin :: (PosixLike m) => String -> Maybe (ShellUtility m)
