{-
Copyright 2013 Google Inc. All Rights Reserved.

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
    This is the "boot" file for Plush.Run.Script. It is used to break a module
    include cycle for GHC.

    Execution involves expansion, including command substition. Of course,
    command substition involves execution, which it does by calling execute.
-}

module Plush.Run.Execute (
    execute,
)
where

import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Types

execute :: (PosixLike m) => CommandList -> ShellExec m ExitCode
