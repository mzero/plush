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

module Plush.Run.BuiltIns.Evaluation (
    eval
    )
where

import Data.List (intercalate)

import Plush.Parser
import Plush.Run.BuiltIns.Utilities
import Plush.Run.Execute
import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Run.Types


eval :: (PosixLike m) => SpecialUtility m
eval = SpecialUtility . const $ Utility evalExec emptyAnnotate
  where
    evalExec args = let cmdline = dropWhile (== ' ') $ intercalate " " args in
        if null cmdline
            then success
            else run cmdline

    run cmdline = do
        aliases <- getAliases
        case parseCommand aliases cmdline of
            Left errs -> exitMsg 127 errs
            Right (cl, _rest) -> shellExec cl >> getLastExitCode

