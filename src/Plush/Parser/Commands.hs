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
    try (more_list ao) <|> return ([],ao)
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
    and_or_op = try (tok_and_if <|> tok_or_if) <&- linebreak

pipeline :: ShellParser (Sense, Pipeline)
pipeline = option Normal tok_bang <&> pipe_sequence

pipe_sequence :: ShellParser Pipeline
pipe_sequence = command `sepBy1` (operator "|" () -&> linebreak)

command :: ShellParser Command
command = simple_command
        <|> (compound_command <&- optional redirect_list)
        <|> function_defintion
compound_command = unexpected "compound commands not yet supported"
function_defintion = unexpected "function definitions not yet supported"

simple_command :: ShellParser Command
simple_command
    =   (cmd_prefix <++> moptional (cmd_word <++> moptional cmd_suffix))
    <|> (cmd_name <++> moptional cmd_suffix)

cmd_name = commandWord <$> tok_word -- apply rule 7a
cmd_word = commandWord <$> tok_word -- apply rule 7b
cmd_prefix = mconcat <$>
    many1 (commandRedirect <$> io_redirect
            <|> commandAssignment <$> tok_assignment_word)
cmd_suffix = mconcat <$>
    many1 (commandRedirect <$> io_redirect
            <|> commandWord <$> tok_word)

redirect_list :: ShellParser [Redirect]
redirect_list = many1 io_redirect

io_redirect :: ShellParser Redirect
io_redirect = do
    n <- option Nothing $ Just <$> tok_io_number
    (r,w) <- (io_file <|> io_here)
    return $ Redirect n r w

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
filename = tok_word -- apply rule 2

io_here = (tok_dless <|> tok_dlessdash) <&> here_end
here_end = tok_word -- apply rule 3

newline_list = many1 tok_newline >> return ()
linebreak = optional newline_list
separator_op = operator "&" Background <|> operator ";" Sequential
separator = (separator_op <&- linebreak) <|> (newline_list >> return Sequential)
-- separator_sep = (operator ";" () -&> linebreak) <|> newline_list

