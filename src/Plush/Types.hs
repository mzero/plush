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

module Plush.Types where

import Data.Hashable
import Data.Monoid

{-
data Token
    = Newline
    | Operator String
    | IoNumber Int
    | Token String  -- could be any of the four below, context dependent
    | Word String
    | Name String
    | AssignmentWord String
    | ReservedWord String
  deriving (Eq, Show)

-}



type CommandList = [(AndOrList, Execution)]
data Execution = Sequential | Background    deriving (Eq, Show)

type AndOrList = [(Connector, (Sense, Pipeline))]
data Connector = AndThen | OrThen           deriving (Eq, Show)
data Sense = Normal | Inverted              deriving (Eq, Show)

type Pipeline = [Command]

data Command = Command [Word] [Assignment] [Redirect]
    deriving (Eq, Show)

instance Monoid Command where
    mempty = Command [] [] []
    mappend (Command wa aa ra) (Command wb ab rb)
        = Command (wa ++ wb) (aa ++ ab) (ra ++ rb)

commandWord :: Word -> Command
commandAssignment :: Assignment -> Command
commandRedirect :: Redirect -> Command

commandWord w       = Command [w] []  []
commandAssignment a = Command []  [a] []
commandRedirect r   = Command []  []  [r]

data Assignment = Assignment String Word
    deriving (Eq, Show)

data Redirect = Redirect (Maybe Int) RedirectType Word
    deriving (Eq, Show)

data RedirectType
    = RedirInput  -- < (def. 0)
    | RedirOutput -- > (def. 1)
    | RedirOutputClobber -- >| (def. 1)
    | RedirAppend -- >> (def .1)
    | RedirHere -- << (def 0)
    | RedirHereStrip -- <<- (def. 0)
    | RedirDuplicateInput -- <& (def. 0)
    | RedirDuplicateOutput -- >& (def. 1)
    | RedirInputOutput -- <> (def. 0)
    deriving (Eq, Show)

data Word = Word { location :: Location, parts :: Parts }
    deriving (Eq, Show)

type Parts = [WordPart]
data WordPart = Bare String
              | Backslashed Char
              | Singlequoted String
              | Doublequoted Parts
              | Parameter String (Maybe (String,Parts)) -- TODO: Modifier type?
              | Subcommand CommandList
              | Arithmetic Parts
              | Expanded String
    deriving (Eq, Show)

wordText :: Word -> String
wordText = concatMap partText . parts

partText :: WordPart -> String
partText (Bare s) = s
partText (Backslashed c) | c == '\n' = ""
                         | otherwise = [c]
partText (Singlequoted s) = s
partText (Doublequoted ps) = concatMap partText ps
partText (Parameter n mm) = "${" ++ n ++ modText mm ++ "}"
  where
    modText Nothing = ""
    modText (Just (m,ps)) = m ++ concatMap partText ps

partText (Subcommand _cl) = "$(...command...)" -- TODO
partText (Arithmetic ps) = "$((" ++ concatMap partText ps ++ "))"

partText (Expanded s) = s


-- | Source location of a parsed construct. References all the way back to the
-- original source text. Character positions are enumerated characters from 1.
data Location = Span Int Int  -- ^ character postion [start,end)
    deriving (Eq, Ord, Show)

instance Hashable Location where
    hash (Span s e) = combine (hash s) (hash e)


modifyParts :: (Parts -> Parts) -> Word -> Word
modifyParts f (Word l p) = Word l $ f p

expandParts :: (Parts -> [Parts]) -> Word -> [Word]
expandParts f (Word l p) = map (Word l) $ f p

modifyPartsM :: (Functor m) => (Parts -> m Parts) -> Word -> m Word
modifyPartsM mf (Word l p) = Word l `fmap` mf p

expandPartsM :: (Functor m) => (Parts -> m [Parts]) -> Word -> m [Word]
expandPartsM mf (Word l p) = map (Word l) `fmap` mf p

