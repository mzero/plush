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

module Plush.Run.BuiltIns.Control (
    break_,
    continue_,
    exit_,
    return_,
    )
where

import Control.Applicative ((<$>))

import Plush.Run.BuiltIns.Utilities
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.ShellExec
import Plush.Run.Types
import Plush.Utilities


break_ :: (PosixLike m) => SpecialUtility m
break_ = SpecialUtility . const $
    Utility (loopControl StBreak) emptyAnnotate

continue_ :: (PosixLike m) => SpecialUtility m
continue_ = SpecialUtility . const $
    Utility (loopControl StContinue) emptyAnnotate

loopControl :: (PosixLike m) =>
    (Int -> ShellStatus) -> Args -> ShellExec m ShellStatus
loopControl st = oneIntArg $ \arg -> case arg of
    Nothing         -> return $ st 1
    Just n | n >= 1 -> return $ st n
    _               -> exitMsg 127 "argument not >= 1"


exit_ :: (PosixLike m) => SpecialUtility m
exit_ = SpecialUtility . const $
    Utility (unwindControl StExit) emptyAnnotate

return_ :: (PosixLike m) => SpecialUtility m
return_ = SpecialUtility . const $
    Utility (unwindControl StReturn) emptyAnnotate

unwindControl :: (PosixLike m) =>
    (ExitCode -> ShellStatus) -> Args -> ShellExec m ShellStatus
unwindControl st = oneIntArg $ \arg -> case arg of
    Nothing                     -> st <$> getLastExitCode
    Just n | 0 <= n && n <= 255 -> return $ st $ intToExitCode n
    _   -> exitMsg 127 "argument not between 0 and 255"


oneIntArg :: (PosixLike m) =>
    (Maybe Int -> ShellExec m ShellStatus) -> Args -> ShellExec m ShellStatus
oneIntArg act args = case args of
    [] -> act Nothing
    [nStr] -> case readMaybe nStr of
        Just n -> act (Just n)
        Nothing -> exitMsg 127 "argument not an integer"
    _ -> exitMsg 127 "two many arugments"
