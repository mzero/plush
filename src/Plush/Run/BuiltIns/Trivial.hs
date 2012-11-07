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
    true, false,
    echo,
    recho, rechoExec,
    tr,
    )
where


import Data.List (intercalate)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import Plush.Run.BuiltIns.Utilities
import Plush.Run.Posix
import Plush.Run.BuiltIns.Syntax
import Plush.Run.Types


true :: (PosixLike m) => DirectUtility m
true = DirectUtility . const $ Utility trueExec noArgsAnnotate
  where
    trueExec _ = success

false :: (PosixLike m) => DirectUtility m
false = DirectUtility . const $ Utility falseExec noArgsAnnotate
  where
    falseExec _ = failure

echo :: (PosixLike m) => BuiltInUtility m
echo = BuiltInUtility $ stdSyntax options "" go
  where
    options = [ flag 'n' ]  -- no newline
    go "n" args = echoExec args ""
    go _ args = echoExec args "\n"
    echoExec args endl = outStr (intercalate " " args ++ endl) >> success

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

tr :: (PosixLike m) => BuiltInUtility m
tr = BuiltInUtility $ stdSyntax options "" go
  where
    options = [ flag 'C', flag 'c', flag 'd', flag 's' ]

    go flags args = do
        contents <- LT.decodeUtf8 `fmap` readAll stdInput
        trExec flags args contents

    trExec "d" [chars] contents = do
        let delSet = S.fromList chars
        outStr $ LT.foldr (delete delSet) LT.empty contents
        success
    trExec (_:_) _args _ = notSupported "tr with flags"
    trExec "" [fromChars, toChars] contents = do
        let trMap = M.fromList $ zip fromChars toChars
        outStr $ LT.map (translate trMap) contents
        success
    trExec _ _ _ = exitMsg 1 "invalid arguments: tr"

    delete delSet c acc = if S.member c delSet then acc else LT.cons c acc
    translate trMap c = M.lookupDefault c c trMap
