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

module Plush.Parser.Tokens (
    whitespace,

    tok_word, tok_name,
    tok_assignment_word,
    tok_io_number,
    tok_newline,

    hereDoc,

    operator,
    tok_and_if, tok_or_if,
    tok_dless, tok_dgreat, tok_lessand, tok_greatand, tok_lessgreat,
    tok_dlessdash, tok_clobber,
    tok_bang,
    tok_lparen, tok_rparen, tok_lbrace, tok_rbrace,

    -- * keywords
    tok_if, tok_then, tok_else, tok_elif, tok_fi, tok_do, tok_done, tok_case,
    tok_esac, tok_while, tok_until, tok_for, tok_in,
    reservedWords,

    content
    )
where

import Control.Applicative ((<*), (<*>), (*>))
import Control.Monad
import qualified Data.Char as Char
import Data.Functor
import Text.Parsec

import Plush.Parser.Aliases (originalSourceColumn)
import Plush.Parser.Base
import Plush.Types
import Plush.Utilities


-- This is a complete refactoring of §2.3 & §2.10.1. Those sections are written
-- operationally for a state machine lexer producing a token stream. However,
-- in places a full parse of the grammar is required in order to lex. Hence,
-- this code, like other implementations, implement the tokenization as part of
-- the grammar. This requires refactoring the tokenization rules into a grammar.

whitespace :: ShellParser ()
whitespace = do
    skipMany (satisfy isBlank)
    optional $ char '#' >> skipMany (satisfy (/= '\n'))

tokenize :: ShellParser a -> ShellParser a
tokenize p = try p <* whitespace

operators :: [String]
operators = words "&& || ;; << >> <& >& <> <<- >|"

tok_word :: ShellParser Word
tok_word = tokenize . locatedWord $
    many1 (backslash <|> singlequote <|> doublequote <|> dollar <|> backquote <|> bare)

-- In the shell command language, a word consisting solely of underscores,
-- digits, and alphabetics from the portable character set. The first character
-- of a name is not a digit.
tok_name :: ShellParser Name
tok_name = tokenize $ do
    start <- originalSourceColumn
    x <- satisfy $ \c -> Char.isAscii c && (Char.isLetter c || c == '_')
    xs <- many $ satisfy $ \c ->
        Char.isAscii c && (Char.isLetter c || Char.isDigit c || c == '_')
    end <- originalSourceColumn
    let loc = Span start end
    return $ Name loc (x:xs)

backslash :: ShellParser WordPart
backslash = char '\\' *> (Backslashed <$> anyChar)

singlequote :: ShellParser WordPart
singlequote = q *> (Singlequoted <$> manyTill anyChar q)
  where q = char '\''

doublequote :: ShellParser WordPart
doublequote = q *> (Doublequoted <$> manyTill dqContent q)
  where
    q = char '"' -- '"' screws up my editor's hilighting

    dqContent = dollar <|> backquote <|> dqBackslash <|> dqBare
    dqBackslash = char '\\' *>
        ( ( Backslashed <$> oneOf "$`\"\\\n" ) <|> return (Bare "\\") )

    dqBare = Bare <$> many1 (noneOf "$`\"\\")
        -- TODO: might be safer as: Bare <$> anyChar, though less efficient

dollar :: ShellParser WordPart
dollar = char '$' *> ( parameter <|> arithmetic <|> subcommand <|> variable )
  where
    parameter = between (char '{') (char '}') $
        (char '#' >> (flip Parameter PModLength) <$> variableName)
        <|>
        (Parameter <$> variableName <*> option PModNone modification)

    arithmetic = between (try $ string "((") (string "))") $
        Arithmetic <$> wordContent (string "))")

    subcommand = between (char '(') (char ')') $
        Subcommand <$> unexpected "subcommands not yet supported" -- complete_command

    variable = variableName >>= \v -> return $ Parameter v PModNone

    variableName = positionalName <|> specialName <|> shellVariableName
    positionalName = many1 digit
        -- these overlap with 0 in special, but the parse is the same
        -- same as dash: $11 is treated as ${11} (bash inerprets it as ${1}1)
    specialName = oneOf "@*#?-$!0" >>= return . (:[])
    shellVariableName =
        liftM2 (:) (char '_' <|> letter) $ many (char '_' <|> alphaNum)

    modification = modifiers <*> wordContent (char '}')
    modifiers = choice $ map try
        [ string ":-" >> return (PModUseDefault True)
        , string "-"  >> return (PModUseDefault False)
        , string ":=" >> return (PModAssignDefault True)
        , string "="  >> return (PModAssignDefault False)
        , string ":?" >> return (PModIndicateError True)
        , string "?"  >> return (PModIndicateError False)
        , string ":+" >> return (PModUseAlternate True)
        , string "+"  >> return (PModUseAlternate False)
        , string "%%" >> return (PModRemoveSuffix True)
        , string "%"  >> return (PModRemoveSuffix False)
        , string "##" >> return (PModRemovePrefix True)
        , string "#"  >> return (PModRemovePrefix False)
        ]
        -- TODO rather inefficient way to do this!

backquote :: ShellParser WordPart
backquote = bq *> (Subcommand <$> manyTill (bqBackslash <|> bqChar) bq)
  where
    bq = char '`'
    bqBackslash = char '\\' *> (oneOf "$`\\" <|> return '\\')
    bqChar = anyChar

-- like tok_word, but a) spaces are allowed, and b) terminates on end
wordContent :: ShellParser a -> ShellParser Word
wordContent endP = locatedWord bits
  where
    bits = (lookAhead (try endP) >> return []) <|> liftM2 (:) bit bits
    bit = (backslash <|> singlequote <|> doublequote <|> dollar <|> bChar)
    bChar = (Bare . (:[])) <$> anyChar

-- | Content that is processed for parameter, command, and arithmetic
-- expansions. Used to process @ENV@ and @PSn@ variables, and also some
-- here-documents. It is very much like the content of doublequoted text.
-- See §2.7.4
content :: ShellParser Parts
content = many (cDollar <|> backquote <|> cBackslash <|> cBare)
  where
    cBackslash = char '\\' *>
        ( ( Backslashed <$> oneOf "$`\\\n" ) <|> return (Bare "\\") )
    cDollar = try dollar <|> (Bare . (:[]) <$> char '$')
    cBare = Bare <$> many1 (noneOf "$`\\")

bare :: ShellParser WordPart
bare = Bare <$> many1 (noneOf nonWordChars)
  where
    nonWordChars = " \t\n\\\'\"$`" ++ operatorStarts
    operatorStarts = concatMap (take 1) operators


locatedWord :: ShellParser Parts -> ShellParser Word
locatedWord partsParser = do
    start <- originalSourceColumn
    ps <- partsParser
    end <- originalSourceColumn
    return $ Word (Span start end) ps
        -- TODO: should handle case where start and end are on differnt lines


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
    parseQueuedHereDocs

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

hereDoc :: HereDocPreprocess -> ([String] -> HereDoc) -> String
    -> ShellParser HereDoc
hereDoc prep makeHereDoc marker = nextHereDoc parseHereDoc
  where
    parseHereDoc = makeHereDoc <$> linesUntilMarker

    linesUntilMarker = (eof >> return []) <|> do
        line <- removeTabs <$> manyTill anyChar (char '\n')
        if line == marker
            then return []
            else (line :) <$> linesUntilMarker
        -- Note: The spec is silent on what to do when the marker is absent.
        -- While an causing a parser error might seem reasonable, it is very
        -- difficult because this parser is called from others wrapped in try
        -- and it isn't possible to break out of a try in Parsec. On the other
        -- hand, other shells just do what we do here: sliently slew to the end.

    removeTabs (c:cs) | prep == HereDocStripTabs && c == '\t' = removeTabs cs
    removeTabs cs = cs

tok_and_if = operator "&&" AndThen
tok_or_if = operator "||" OrThen
-- tok_demi = operator ";;"

tok_dgreat = operator ">>" RedirAppend
tok_lessand = operator "<&" RedirDuplicateInput
tok_greatand = operator ">&" RedirDuplicateOutput
tok_lessgreat = operator "<>" RedirInputOutput
tok_clobber = operator ">|" RedirOutputClobber

data HereDocPreprocess = HereDocPlain | HereDocStripTabs
    deriving (Eq)

tok_dless = operator "<<" HereDocPlain
tok_dlessdash = operator "<<-" HereDocStripTabs


tok_bang :: ShellParser Sense
tok_bang = tokenize $ char '!' >> return Inverted

tok_lparen = tokenize $ char '('
tok_rparen = tokenize $ char ')'
tok_lbrace = tokenize $ char '{'
tok_rbrace = tokenize $ char '}'

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
    words "if then else elif fi do done case esac while until for { } ! in"
