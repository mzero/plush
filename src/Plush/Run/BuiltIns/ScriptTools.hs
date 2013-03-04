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

module Plush.Run.BuiltIns.ScriptTools (
    read_,
    )
where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.Char (isSpace)
import Data.List (partition)

import Plush.Run.BuiltIns.Utilities
import Plush.Run.BuiltIns.Syntax
import Plush.Run.Posix
import Plush.Run.Posix.Return
import Plush.Run.Posix.Utilities
import Plush.Run.ShellExec
import Plush.Run.Types

read_ :: (PosixLike m) => DirectUtility m
read_ = DirectUtility $ stdSyntax options "" go
  where
    options = [ flag 'r' ]

    go _ [] = exitMsg 127 "read: no variable names supplied"
    go flags names = do
        let rFlag = 'r' `elem` flags
        (inErr, input) <- getInput rFlag
        ifs <- getVarDefault "IFS" " \t\n"
        let bindings = fieldSplit rFlag ifs names input
        bindVars setShellVar bindings
            >>= ifError (return . errorExitCode)
                        (if inErr then failure else success)

    -- read's field splitting is defined with reference to section §2.6.5
    -- Field Splitting. However, the specification for read differs in
    -- significant ways:
    --      * yet another form of backslash quoting
    --      * a non-quoting mode (the -r flag)
    --      * handling of leading and trailing whitespace
    --      * specialing handling when there are more fields than variables
    --      * line continuation handling
    -- About the only thing that is the same is the handling of IFS.
    -- Therefore, alas, this code is a separate implementation.

    fieldSplit rFlag ifs names = assignNext names . dropWhite
      where
        assignNext [] _ = []
        assignNext [n] s = [(n, dropTrailingWhite s)]
        assignNext (n:ns) s = let (w, r) = breakField "" s
                              in (n, w) : assignNext ns (dropSeparator r)

        breakField rf ""                                = (reverse rf, "")
        breakField rf "\\"     | not rFlag              = (reverse rf, "")
        breakField rf (c:d:cs) | not rFlag && c == '\\' = breakField (d:rf) cs
        breakField rf s@(c:cs) | c `elem` ifs           = (reverse rf, s)
                               | otherwise              = breakField (c:rf) cs

        dropWhite = dropWhile (`elem` ifsWhite)
        dropSeparator s = case dropWhite s of
            (c:cs) | c `elem` ifsSeparator -> dropWhite cs
            s' -> s'
        dropTrailingWhite = reverse . dropWhite . reverse

        (ifsWhite, ifsSeparator) = partition isSpace ifs

    getInput rFlag = do
        (err, bs) <- readLine stdInput
        let line = fromByteString bs
        if rFlag
            then return (err, deNewline line)
            else case deContinue "" line of
                Just line' | not err -> second (line' ++) <$> getInput rFlag
                _                    -> return (err, deNewline line)

    deNewline "" = ""
    deNewline "\n" = ""
    deNewline (c:cs) = c : deNewline cs

    deContinue _ [] = Nothing
    deContinue rs "\\\n" = Just $ reverse rs
    deContinue rs ('\\':c:cs) = deContinue (c:'\\':rs) cs
    deContinue rs (c:cs) = deContinue (c:rs) cs
