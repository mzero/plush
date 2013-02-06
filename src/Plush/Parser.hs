{-
Copyright 2012-2013 Google Inc. All Rights Reserved.

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
    parseCommand,
    ParseCommandResult,
)
where

import Control.Applicative ((*>))
import Control.Arrow (second, (+++))
import Text.Parsec

import Plush.Parser.Aliases
import Plush.Parser.Base
import Plush.Parser.Commands
import Plush.Parser.Tokens
import Plush.Types

-- | Either it is an error message, or a parsed command list, and the remainder
-- of the input string.
type ParseCommandResult = Either String (CommandList, String)

-- | Parse the next "commplete_command" in the input, and return it and the
-- remainder of the input, or an error message
parseCommand :: Aliases -> String -> ParseCommandResult
parseCommand aliases = (show +++ unprep) . shellParse nextCommand "" . prep
  where
    nextCommand = (whitespace >> linebreak *> complete_command) <&> getInput

    prep = dealiasStream aliases
    unprep = second dealiasRemainder
