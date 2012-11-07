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

module Plush.Parser.Commands (
    complete_command,
    linebreak
    )
where
import qualified Control.Applicative as Applicative
import Control.Applicative ((<*), (*>), (<*>))
import Data.Functor
import Data.Monoid
import Text.Parsec

import Plush.Parser.Base
import Plush.Parser.Tokens
import Plush.Types

-- A close translation of the Shell Grammar from §2.10

-- complete_command and list are somewhat awkward because they are matching
-- the spec, which parses these constructs in an odd way: The final optional
-- separator is parsed by complete_command, though it determines the execution
-- of the last and_or parsed by list.

complete_command :: ShellParser CommandList
complete_command = do
    (cl, ao) <- list
    s <- option Sequential separator
    return $ cl ++ [(ao,s)]

list :: ShellParser (CommandList, AndOrList) -- leading list & last and_or
list = do
    ao <- and_or
    try (more_list ao) <|> return ([], ao)
  where
    more_list ao = do
        s <- separator_op
        (cl, ao') <- list
        return ((ao,s):cl, ao')

and_or :: ShellParser AndOrList
and_or = do
    first <- pipeline
    rest <- many $ and_or_op <&> pipeline
    return $ (AndThen, first) : rest
  where
    and_or_op = try (tok_and_if <|> tok_or_if) <* linebreak

pipeline :: ShellParser (Sense, Pipeline)
pipeline = option Normal tok_bang <&> pipe_sequence

pipe_sequence :: ShellParser Pipeline
pipe_sequence = command `sepBy1` (operator "|" () *> linebreak)

command :: ShellParser Command
command = (Compound <$> compound_command <*> option [] redirect_list)
    <|> Function <$> function_defintion
    <|> Simple <$> simple_command

-- * compound command

compound_command :: ShellParser CompoundCommand
compound_command = choice
    [ brace_group, subshell, for_clause, case_clause, if_clause
    , while_clause, until_clause
    ]

brace_group :: ShellParser CompoundCommand
brace_group = unexpected "brace_group not supported"

subshell :: ShellParser CompoundCommand
subshell = unexpected "subshell not supported"

for_clause :: ShellParser CompoundCommand
for_clause = do
    -- Apply rule 5 for 'name', but basically it just means it has to look
    -- like a name and not any old word (name requires a more restricted
    -- character set).
    name <- tok_for *> tok_name <* linebreak
    words_ <- optionMaybe $ tok_in *> many tok_word <* sequential_sep
    doGroup <- tok_do *> compound_list <* tok_done
    return $ ForClause name words_ doGroup

case_clause :: ShellParser CompoundCommand
case_clause = unexpected "case_clause not supported"

if_clause :: ShellParser CompoundCommand
if_clause = unexpected "if_clause not supported"

while_clause :: ShellParser CompoundCommand
while_clause = unexpected "while_clause not supported"

until_clause :: ShellParser CompoundCommand
until_clause = unexpected "until_clause not supported"

-- | This is the same as 'list', except that the cmds are separated with
-- 'separator' instead of 'separator_op'.  Normally I'd factor them into one
-- function, but we're trying to maintain exactly the structure and names of
-- the POSIX grammar so copy paste.
compound_list :: ShellParser CommandList
compound_list = do
    (cl, ao) <- term
    s <- option Sequential separator
    return $ cl ++ [(ao, s)]

term :: ShellParser (CommandList, AndOrList)
term = do
    ao <- and_or
    try (more_list ao) <|> return ([], ao)
  where
    more_list ao = do
        s <- separator
        (cl, ao') <- list
        return ((ao,s):cl, ao')

-- * function definition

function_defintion :: ShellParser FunctionDefinition
function_defintion = unexpected "function definitions not yet supported"

-- * simple command

simple_command :: ShellParser SimpleCommand
simple_command
    =   (cmd_prefix <++> moptional (cmd_word <++> moptional cmd_suffix))
    <|> (cmd_name <++> moptional cmd_suffix)

cmd_name :: ShellParser SimpleCommand
cmd_name = notReservedWord >> (commandWord <$> tok_word) -- apply rule 7a

-- | This is rule 1.
--
-- Fail if this is a reserved word.
notReservedWord :: ShellParser ()
notReservedWord = notFollowedBy $ do
    word <- tok_word
    case parts word of
        [Bare s] | s `elem` reservedWords -> return $ "keyword: " ++ s
        _ -> Applicative.empty

cmd_word :: ShellParser SimpleCommand
cmd_word = commandWord <$> tok_word -- apply rule 7b

cmd_prefix :: ShellParser SimpleCommand
cmd_prefix = mconcat <$>
    many1 (commandRedirect <$> io_redirect
            <|> commandAssignment <$> tok_assignment_word)

cmd_suffix :: ShellParser SimpleCommand
cmd_suffix = mconcat <$>
    many1 (commandRedirect <$> io_redirect
            <|> commandWord <$> tok_word)

redirect_list :: ShellParser [Redirect]
redirect_list = many1 io_redirect

io_redirect :: ShellParser Redirect
io_redirect = do
    n <- option Nothing $ Just <$> tok_io_number
    (r, w) <- io_file <|> io_here
    return $ Redirect n r w

io_file :: ShellParser (RedirectType, Word)
io_file = choice fileOps <&> filename
  where
    fileOps =
        [ operator "<" RedirInput
        , tok_lessand
        , operator ">" RedirOutput
        , tok_greatand
        , tok_dgreat
        , tok_lessgreat
        , tok_clobber
        ]

filename :: ShellParser Word
filename = tok_word -- apply rule 2

io_here :: ShellParser (RedirectType, Word)
io_here = (tok_dless <|> tok_dlessdash) <&> here_end

here_end :: ShellParser Word
here_end = tok_word -- apply rule 3

newline_list :: ShellParser ()
newline_list = many1 tok_newline >> return ()

linebreak :: ShellParser ()
linebreak = optional newline_list

separator_op :: ShellParser Execution
separator_op = operator "&" Background <|> operator ";" Sequential

separator :: ShellParser Execution
separator = (separator_op <* linebreak) <|> (newline_list >> return Sequential)

sequential_sep :: ShellParser ()
sequential_sep = (operator ";" () *> linebreak) <|> newline_list

