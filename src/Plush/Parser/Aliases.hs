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

{-# Language MultiParamTypeClasses, FlexibleInstances #-}

module Plush.Parser.Aliases (
    -- * Aliases
    Aliases,
    aliasNameChar,
    )
where

import Data.Char (isAlpha, isDigit)
import qualified Data.HashMap.Strict as M


type Aliases = M.HashMap String String

aliasNameChar :: Char -> Bool
aliasNameChar '_' = True
aliasNameChar c | isDigit c = True
aliasNameChar c | isAlpha c = True
aliasNameChar '!' = True
aliasNameChar '%' = True
aliasNameChar ',' = True
aliasNameChar '@' = True
aliasNameChar _ = False
