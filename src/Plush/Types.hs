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

module Plush.Types where

import Data.Hashable
import Data.Monoid


type CommandList = [(AndOrList, Execution)]
data Execution = Sequential | Background    deriving (Eq, Show)

type AndOrList = [(Connector, (Sense, Pipeline))]
data Connector = AndThen | OrThen           deriving (Eq, Show)
data Sense = Normal | Inverted              deriving (Eq, Show)

type Pipeline = [Command]

data Command =
    Simple SimpleCommand
    | Compound CompoundCommand [Redirect]
    | Function Name FunctionBody
    deriving (Eq, Show)

data FunctionBody = FunctionBody CompoundCommand [Redirect]
    deriving (Eq, Show)

data SimpleCommand = SimpleCommand [Word] [Assignment] [Redirect]
    deriving (Eq, Show)

instance Monoid SimpleCommand where
    mempty = SimpleCommand [] [] []
    mappend (SimpleCommand wa aa ra) (SimpleCommand wb ab rb)
        = SimpleCommand (wa ++ wb) (aa ++ ab) (ra ++ rb)

data CompoundCommand =
    BraceGroup CommandList
    | Subshell CommandList

    | ForLoop Name (Maybe [Word]) CommandList
    -- ^ There is a difference between the word list being absent (when there
    -- is no @in@ keyword) and the word list being empty (when @in@ is present).
    -- Hence the the 'Maybe'.

    | IfConditional [(CommandList, CommandList)] (Maybe CommandList)
    -- ^ The initial @if@ clause and any @elif@ clauses are all compiled into
    -- the list of (conditional, consequent) command lists. The final optional
    -- 'CommandList' is the @else@ part.

    | WhileLoop CommandList CommandList
    | UntilLoop CommandList CommandList

    | CaseConditional Word [([Word], Maybe CommandList)]
    deriving (Eq, Show)

commandWord :: Word -> SimpleCommand
commandAssignment :: Assignment -> SimpleCommand
commandRedirect :: Redirect -> SimpleCommand

commandWord w       = SimpleCommand [w] []  []
commandAssignment a = SimpleCommand []  [a] []
commandRedirect r   = SimpleCommand []  []  [r]

data Assignment = Assignment String Word
    deriving (Eq, Show)

data Redirect = Redirect (Maybe Int) RedirectType Word
              | RedirectHere (Maybe Int) HereDoc Word -- << and <<- (def. 0)
    deriving (Eq, Show)

data RedirectType
    = RedirInput  -- < (def. 0)
    | RedirOutput -- > (def. 1)
    | RedirOutputClobber -- >| (def. 1)
    | RedirAppend -- >> (def .1)
    | RedirDuplicateInput -- <& (def. 0)
    | RedirDuplicateOutput -- >& (def. 1)
    | RedirInputOutput -- <> (def. 0)
    deriving (Eq, Show)

data HereDoc = HereDocLiteral [String]
             | HereDocParsed [String]
             | HereDocMissing
    deriving (Eq, Show)

data ParameterModifier
    = PModNone
    | PModUseDefault Bool Word      -- - and :-
    | PModAssignDefault Bool Word   -- = and :=
    | PModIndicateError Bool Word   -- ? and :?
    | PModUseAlternate Bool Word    -- + and :+
    | PModLength                    -- #
    | PModRemoveSuffix Bool Word    -- % and %%
    | PModRemovePrefix Bool Word    -- # and ##
    deriving (Eq, Show)

data Word = Word { location :: Location, parts :: Parts }
    deriving (Eq, Show)

-- | Simple (unexpanded) names, such as in for loops and function declarations.
data Name = Name Location String
    deriving (Eq, Show)

type Parts = [WordPart]
data WordPart = Bare String
              | Backslashed Char
              | Singlequoted String
              | Doublequoted Parts
              | Parameter String ParameterModifier
              | Commandsub CommandList
              | Backquoted String
              | Arithmetic Word
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
partText (Parameter n mm) = "${" ++ modPre mm ++ n ++ modPost mm ++ "}"
  where
    modPre PModLength = "#"
    modPre _ = ""

    modPost PModNone = ""
    modPost (PModUseDefault    True  wd) = ':' : '-' : wordText wd
    modPost (PModUseDefault    False wd) =       '-' : wordText wd
    modPost (PModAssignDefault True  wd) = ':' : '=' : wordText wd
    modPost (PModAssignDefault False wd) =       '=' : wordText wd
    modPost (PModIndicateError True  wd) = ':' : '?' : wordText wd
    modPost (PModIndicateError False wd) =       '?' : wordText wd
    modPost (PModUseAlternate  True  wd) = ':' : '+' : wordText wd
    modPost (PModUseAlternate  False wd) =       '+' : wordText wd
    modPost PModLength = ""
    modPost (PModRemoveSuffix  True  wd) = '%' : '%' : wordText wd
    modPost (PModRemoveSuffix  False wd) =       '%' : wordText wd
    modPost (PModRemovePrefix  True  wd) = '#' : '#' : wordText wd
    modPost (PModRemovePrefix  False wd) =       '#' : wordText wd

partText (Commandsub _cl) = "$(" ++ "--command substituion--" ++ ")"
partText (Backquoted s) = "`" ++ s ++ "`"
partText (Arithmetic wd) = "$((" ++ wordText wd ++ "))"

partText (Expanded s) = s

compress :: Parts -> Parts
compress ((Bare s):(Bare t):ps) = compress $ Bare (s ++ t) : ps
compress ((Expanded s):(Expanded t):ps) = compress $ (Expanded (s ++ t)) : ps
compress (p:ps) = p : compress ps
compress [] = []

quoteRemoval :: Word -> String
quoteRemoval = concatMap qp . parts
  where
    qp (Backslashed '\n') = ""  -- should this be handled here or in the parse?
    qp (Backslashed c) = [c]
    qp (Singlequoted s) = s
    qp (Doublequoted ps) = concatMap qp ps
    qp p = partText p

hasQuotedText :: Word -> Bool
hasQuotedText = any quotedPart . parts
  where
    quotedPart (Backslashed _) = True
    quotedPart (Singlequoted _) = True
    quotedPart (Doublequoted _) = True
    quotedPart _ = False


-- | Source location of a parsed construct. References all the way back to the
-- original source text. Character positions are enumerated characters from 1.
data Location = Span Int Int  -- ^ character postion [start,end)
    deriving (Eq, Ord, Show)

instance Hashable Location where
    salt `hashWithSalt` (Span s e) = salt `hashWithSalt` s `hashWithSalt` e


modifyParts :: (Parts -> Parts) -> Word -> Word
modifyParts f (Word l p) = Word l $ f p

expandParts :: (Parts -> [Parts]) -> Word -> [Word]
expandParts f (Word l p) = map (Word l) $ f p

modifyPartsM :: (Functor m) => (Parts -> m Parts) -> Word -> m Word
modifyPartsM mf (Word l p) = Word l `fmap` mf p

expandPartsM :: (Functor m) => (Parts -> m [Parts]) -> Word -> m [Word]
expandPartsM mf (Word l p) = map (Word l) `fmap` mf p
