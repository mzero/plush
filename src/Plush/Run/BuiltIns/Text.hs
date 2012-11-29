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

module Plush.Run.BuiltIns.Text (
    tr,
    )
where


import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import Plush.Run.BuiltIns.Utilities
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.BuiltIns.Syntax
import Plush.Run.Types


tr :: (PosixLike m) => BuiltInUtility m
tr = BuiltInUtility $ stdSyntax options "" go
  where
    options = [ flag 'd' ]
        -- TODO: implement -c, -C, and -s flags
    go flags args = do
        contents <- T.decodeUtf8With T.lenientDecode `fmap` readAll stdInput
        if 'd' `elem` flags
            then trDelete args contents
            else trReplace args contents


trDelete :: (PosixLike m) => [String] -> T.Text -> m ExitCode
trDelete [delChars] contents =
    outStr (T.concatMap deleteIfIn contents) >> success
  where
    deleteIfIn c = if S.member c deleteSet then T.empty else T.singleton c
    deleteSet = S.fromList $ trExpand delChars

trDelete _ _ = exitMsg 1 "tr -d takes exactly one argument"


trReplace :: (PosixLike m) => [String] -> T.Text -> m ExitCode
trReplace [fromChars, toChars] contents =
    outStr (T.map translate contents) >> success
  where
    translate c = M.lookupDefault c c translateMap
    translateMap = M.fromList $ zip (trExpand fromChars) (trExpand toChars)

trReplace _ _ = exitMsg 1 "tr takes exactly two arguments"


-- | Expand character set arguments to tr into full character enumerations
trExpand :: String -> String
trExpand "" = ""
trExpand (a:'-':b:cs)  = [a..b] ++ trExpand cs
trExpand ('\\':'a':cs) = '\a' : trExpand cs  -- Haskell's character escapes
trExpand ('\\':'b':cs) = '\b' : trExpand cs  -- match the POSIX's. We checked!
trExpand ('\\':'f':cs) = '\f' : trExpand cs
trExpand ('\\':'n':cs) = '\n' : trExpand cs
trExpand ('\\':'r':cs) = '\r' : trExpand cs
trExpand ('\\':'t':cs) = '\t' : trExpand cs
trExpand ('\\':'v':cs) = '\v' : trExpand cs
trExpand ('\\': _ :cs) =        trExpand cs  -- undefined behavior, this drops
trExpand (a       :cs) = a    : trExpand cs
    -- TODO: handle octal escapes
    -- TODO: handle character classes
