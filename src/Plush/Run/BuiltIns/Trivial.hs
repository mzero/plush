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

module Plush.Run.BuiltIns.Trivial (
    colon,
    true, false,
    echo,
    recho, rechoExec,
    )
where


import Data.List (intercalate)

import Plush.Run.BuiltIns.Utilities
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.Types


colon :: (PosixLike m) => SpecialUtility m
colon = SpecialUtility . const $ Utility colonExec noArgsAnnotate
  where
    colonExec _ = success

true :: (PosixLike m) => DirectUtility m
true = DirectUtility . const $ Utility trueExec noArgsAnnotate
  where
    trueExec _ = success

false :: (PosixLike m) => DirectUtility m
false = DirectUtility . const $ Utility falseExec noArgsAnnotate
  where
    falseExec _ = failure

echo :: (PosixLike m) => BuiltInUtility m
echo = BuiltInUtility . const $ Utility echoExec emptyAnnotate
    -- NOTE: can't use stdSyntax here, because defintion of echo is funny
  where
    echoExec ("-n":args) = outStr   (process args) >> success
    echoExec       args  = outStrLn (process args) >> success

    process = intercalate " "

recho :: (PosixLike m) => BuiltInUtility m
recho = BuiltInUtility . const $ Utility rechoExec emptyAnnotate

rechoExec :: (PosixLike m) => Args -> m ExitCode
rechoExec args = mapM_ outStrLn (zipWith argLine [(1::Int)..] args) >> success
  where
    argLine i a = "argv[" ++ show i ++ "] = <" ++ concatMap argChar a ++ ">"
    argChar c
        | c < ' '     = '^' : (toEnum . (+64) . fromEnum) c : []
        | c == '\DEL' = "^?"
        | otherwise   = c : []
