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

module Plush.Run.BuiltIns.WorkingDirectory (
    cd,
    )
where

import Control.Monad (when)
import Data.Maybe (listToMaybe)
import System.FilePath

import Plush.Run.BuiltIns.Syntax
import Plush.Run.BuiltIns.Utilities
import Plush.Run.Posix
import Plush.Run.ShellExec
import Plush.Run.Types

cd :: (PosixLike m) => DirectUtility m
cd = DirectUtility $ stdSyntax options "L" go
  where
    options =
        [ toggle 'L' "LP"   -- treat dot-dot logically
        , toggle 'P' "LP"   -- treat dot-dot physically
        ]

    -- a somewhat literal coding of the spec, who knew it was this complex?!
    go flags args = case args of
        ("-":_) -> getVarDefault "OLDPWD" "" >>= go12 True . Just
        _ -> go12 False $ listToMaybe args
      where
        physical = 'P' `elem` flags

        go12 pr Nothing = getVar "HOME" >>=
            maybe (exitMsg 1 "$HOME not set") (go345 pr)
        go12 pr (Just d) = go345 pr d
        go345 pr d = case splitDirectories d of
            (d0:_) | d0 `elem` ["/", ".", ".."] -> go67 pr d
            _ -> getVarDefault "CDPATH" "" >>= go5' pr d . splitSearchPath
        go5' pr d (p:ps) = do
            let d' = p </> d
            isDir <- doesDirectoryExist d'
            if isDir then go67 (p /= ".") d' else go5' pr d ps
        go5' pr d [] = go67 pr d
        go67 pr d
            | physical = go910 pr d
            | otherwise = getVarDefault "PWD" "" >>= go8 pr . (</> d)
        go8 pr d = go910 pr (simplifyPath d)
        go910 pr d = do
            p <- getVarDefault "PWD" ""
            _ <- setVarEntry "OLDPWD" (VarExported, VarReadWrite, Just p)
            -- Unlike bash and dash, if OLDPWD is marked readonly, we don't fail.
            changeWorkingDirectory d
            _ <- if physical
                 then setVarEntry "PWD" (VarExported, VarReadWrite, Just d)
                      -- TODO: This is wrong, should be same as pwd -P
                 else setVarEntry "PWD" (VarExported, VarReadWrite, Just d)
            when pr $ outStrLn d
            success
