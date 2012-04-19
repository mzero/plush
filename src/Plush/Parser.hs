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

{-# LANGUAGE FlexibleContexts #-}

module Plush.Parser (
    parseNextCommand
)
where

import Control.Arrow (left)
import Text.Parsec

import Plush.Parser.Base
import Plush.Parser.Commands
import Plush.Parser.Tokens
import Plush.Types


-- Parse the next "commplete_command" in the input, and return it and the 
-- remainder of the input, or an error message
parseNextCommand :: String -> Either String (CommandList, String)
parseNextCommand input = left show $ parse nextCommand "" input
  where
    nextCommand :: ShellParser (CommandList, String)
    nextCommand = (whitespace >> linebreak -&> complete_command) <&> getInput


