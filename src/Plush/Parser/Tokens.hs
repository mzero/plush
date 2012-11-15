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

module Plush.Parser.Tokens (
    whitespace,

    tok_word, tok_name,
    tok_assignment_word,
    tok_io_number,
    tok_newline,

    operator,
    tok_and_if, tok_or_if,
    tok_dless, tok_dgreat, tok_lessand, tok_greatand, tok_lessgreat,
    tok_dlessdash, tok_clobber,
    tok_bang,

    -- * keywords
    tok_if, tok_then, tok_else, tok_elif, tok_fi, tok_do, tok_done, tok_case,
    tok_esac, tok_while, tok_until, tok_for, tok_in,
    reservedWords
    )
where

import Control.Applicative ((<*), (*>))
import Control.Monad
import qualified Data.Char as Char
import Data.Functor
import Text.Parsec

import Plush.Parser.Base
import Plush.Types


-- This is a complete refactoring of §2.3 & §2.10.1. Those sections are written
-- operationally for a state machine lexer producing a token stream. However,
-- in places a full parse of the grammar is required in order to lex. Hence,
-- this code, like other implementations, implement the tokenization as part of
-- the grammar. This requires refactoring the tokenization rules into a grammar.

whitespace :: ShellParser ()
whitespace = do
    skipMany (char ' ')
    optional $ char '#' >> skipMany (satisfy (/= '\n'))

tokenize :: ShellParser a -> ShellParser a
tokenize p = try p <* whitespace

operators :: [String]
operators = words "&& || ;; << >> <& >& <> <<- >|"

tok_word :: ShellParser Word
tok_word = tokenize $ do
    start <- getPosition
    ps <- many1 (backslash <|> singlequote <|> doublequote <|> dollar <|> bare)
    end <- getPosition
    return $ Word (Span (sourceColumn start) (sourceColumn end)) ps
        -- TODO: should handle case where start and end are on differnt lines

-- In the shell command language, a word consisting solely of underscores,
-- digits, and alphabetics from the portable character set. The first character
-- of a name is not a digit.
tok_name :: ShellParser Name
tok_name = tokenize $ do
    start <- getPosition
    x <- satisfy $ \c -> Char.isAscii c && (Char.isLetter c || c == '_')
    xs <- many $ satisfy $ \c ->
        Char.isAscii c && (Char.isLetter c || Char.isDigit c || c == '_')
    end <- getPosition
    let loc = Span (sourceColumn start) (sourceColumn end)
    return $ Name loc (x:xs)

backslash :: ShellParser WordPart
backslash = char '\\' *> (Backslashed <$> anyChar)

singlequote :: ShellParser WordPart
singlequote = q *> (Singlequoted <$> manyTill anyChar q)
  where q = char '\''

doublequote :: ShellParser WordPart
doublequote =
    (q *>) $ Doublequoted <$> manyTill (dqBackslash <|> dollar <|> dqBare) q
  where
    q = char '"' -- '"' screws up my editor's hilighting

    dqBackslash = char '\\' *>
        ( ( Backslashed <$> oneOf "$`\"\\\n" ) <|> return (Bare "\\") )

    dqBare = Bare <$> many1 (noneOf "$`\"\\")
        -- TODO: might be safer as: Bare <$> anyChar, though less efficient

dollar :: ShellParser WordPart
dollar = char '$' *> ( parameter <|> arithmetic <|> subcommand <|> variable )
  where
    parameter = between (char '{') (char '}') $
        liftM2 Parameter variableName (optionMaybe modification)

    arithmetic = between (try $ string "((") (string "))") $
        Arithmetic <$> wordContent (string "))")

    subcommand = between (char '(') (char ')') $
        Subcommand <$> unexpected "subcommands not yet supported" -- complete_command

    variable = variableName >>= \v -> return $ Parameter v Nothing

    variableName = positionalName <|> specialName <|> shellVariableName
    positionalName = many1 digit
        -- these overlap with 0 in special, but the parse is the same
        -- same as dash: $11 is treated as ${11} (bash inerprets it as ${1}1)
    specialName = oneOf "@*#?-$!0" >>= return . (:[])
    shellVariableName =
        liftM2 (:) (char '_' <|> letter) $ many (char '_' <|> alphaNum)

    modification = choice modifiers <&> wordContent (char '}')
    modifiers = map (try . string) $ words ":- - := = :? ? :+ + % %% # ##"
        -- TODO rather inefficient way to do this!

-- like tok_word, but a) spaces are allowed, and b) terminates on end
wordContent :: ShellParser a -> ShellParser [WordPart]
wordContent endP = bits
  where
    bits = (lookAhead (try endP) >> return []) <|> liftM2 (:) bit bits
    bit = (backslash <|> singlequote <|> doublequote <|> dollar <|> bChar)
    bChar = (Bare . (:[])) <$> anyChar


bare :: ShellParser WordPart
bare = Bare <$> many1 (noneOf nonWordChars)
  where
    nonWordChars = " \n\\\'\"$" ++ operatorStarts
    operatorStarts = concatMap (take 1) operators


tok_assignment_word :: ShellParser Assignment
tok_assignment_word = tokenize $ do
    v <- char '_' <|> letter
    vs <- many (char '_' <|> alphaNum)
    _ <- char '='
    w <- tok_word
    return $ Assignment (v:vs) w

tok_io_number :: ShellParser Int
tok_io_number = tokenize $ do
    n <- many1 digit
    _ <- lookAhead (char '<' <|> char '>')
    return $ read n

tok_newline :: ShellParser ()
tok_newline = tokenize $ do
    _ <- char '\n'
    return ()

operator :: String -> a -> ShellParser a
operator [] _ = parserZero
operator op@(c0:_) r = tokenize $ do
    _ <- char c0
    maximalOp <- runNext operators c0
    if maximalOp == op
        then return r
        else unexpected maximalOp
  where
    runOne [] = return ""
    runOne ws = (satisfy (anyHead ws) >>= runNext ws) <|> return ""
    runNext ws c = runOne [ as | (a:as) <- ws, a == c ] >>= return . (c:)
    anyHead ws c = any (==c) [ a | (a:_) <- ws ]


tok_and_if = operator "&&" AndThen
tok_or_if = operator "||" OrThen
-- tok_demi = operator ";;"

tok_dless = operator "<<" RedirHere
tok_dgreat = operator ">>" RedirAppend
tok_lessand = operator "<&" RedirDuplicateInput
tok_greatand = operator ">&" RedirDuplicateOutput
tok_lessgreat = operator "<>" RedirInputOutput
tok_dlessdash = operator "<<-" RedirHereStrip
tok_clobber = operator ">|" RedirOutputClobber

tok_bang :: ShellParser Sense
tok_bang = tokenize $ char '!' >> return Inverted

keyword :: String -> ShellParser String
keyword name = tokenize $ string name

tok_if = keyword "if"
tok_then = keyword "then"
tok_else = keyword "else"
tok_elif = keyword "elif"
tok_fi = keyword "fi"
tok_do = keyword "do"
tok_done = keyword "done"
tok_case = keyword "case"
tok_esac = keyword "esac"
tok_while = keyword "while"
tok_until = keyword "until"
tok_for = keyword "for"
tok_in = keyword "in"

reservedWords :: [String]
reservedWords =
    words "if then else elif fi do done case esac while until for in"
