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

module Plush.Run.Expansion.Arithmetic (
    runArithmetic,
    )
where

import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Error (Error(..), ErrorT, runErrorT, throwError)
import Data.Bits
import Data.Char (digitToInt)
import Data.List (foldl')
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String

import Plush.Parser.Tokens (name)
import Plush.Run.Posix
import Plush.Run.Posix.Utilities
import Plush.Run.ShellExec
import Plush.Run.Types
import Plush.Utilities

-- | Parse and evaluate an arithmetic expression. This can both fetch and set
-- shell variables.
runArithmetic :: (PosixLike m) => String -> ShellExec m String
runArithmetic s = case parse (expression <* eof) "" s of
    Left errs -> do
        errStr (show errs)
        return "???"
    Right e -> do
        result <- runErrorT (evaluate e)
        return $ either (const "!!!") show result
    -- TODO(mzero): fix these returns, once we can propigate a bad status

--
-- Evaluation
--

-- | Type of expressions
type ArithType = Int

-- | Expression tree
data Expr = ExConstant ArithType
          | ExUnary (ArithType -> ArithType) Expr
          | ExBinary (ArithType -> ArithType -> ArithType) Expr Expr
          | ExConditional Expr Expr Expr
          | ExFetch String
          | ExStore String Expr

-- | Errors during evalutaion. These are just 'ShellStatus' values from calls to
-- 'shellError'. Rather than throw them in the 'PosixLIke' monad, these will be
-- used in an 'ErrorT' monad wrapper to insulate them from other errors.
newtype EvalError = EvalError ShellStatus
instance Error EvalError where
    noMsg = EvalError $ StError (ExitFailure 1)

-- | Evaluate an expression tree. The extra 'ErrorT' monad layer isolates the
-- errors that can happen here from other errors thrown in 'PosixLike' monad.
evaluate :: (PosixLike m) => Expr -> ErrorT EvalError (ShellExec m) ArithType
evaluate (ExConstant n)     = return n
evaluate (ExUnary f ea)     = f <$> evaluate ea
evaluate (ExBinary f ea eb) = f <$> evaluate ea <*> evaluate eb
evaluate (ExConditional et ea eb) =
    evaluate et >>= (\t -> evaluate $ if t /= 0 then ea else eb)
evaluate (ExFetch var) = do
    mn <- lift (getVar var)
    case readMaybe <$> mn of
        Just (Just n) -> return n
        _ -> do
            st <- lift (shellError 127
                            ("variable " ++ var ++ ": value isn't numeric"))
            throwError $ EvalError st
evaluate (ExStore var e) = do
    n <- evaluate e
    st <- lift $ setVarEntry var (VarShellOnly, VarReadWrite, Just $ show n)
    case st of
        StStatus _ -> return n
        _          -> throwError $ EvalError st


--
-- Parsing
--

-- | The top level non-terminal of an arithmetic expression.
expression :: Parser Expr
expression = whitespace >> assignExpr

term :: Parser Expr
term = tok $
    parens expression <|> number <|> (variable >>= return . ExFetch)
  where
    number = try (string "0x" >> numBase 16 <$> many1 hexDigit)
             <|> (char '0'    >> numBase 8  <$> many  octDigit)
             <|> (               numBase 10 <$> many1 digit)
             <?> "number"
    numBase b = ExConstant . foldl' (\n d -> n * b + digitToInt d) 0
    parens = between (char '(') (char ')')

opExpr :: Parser Expr
opExpr = buildExpressionParser operators term
  where
    operators =
        [ [ prefixOp "-" negate
          , prefixOp "+" id
          , prefixOp "~" complement
          , prefixOp "!" (fromEnum . (==0))
          ]

        , [ binaryOp "*" (*)
          , binaryOp "/" quot
          , binaryOp "%" rem
          ]

        , [ binaryOp "+" (+)
          , binaryOp "-" (-)
          ]

        , [ binaryOp "<<" shiftL
          , binaryOp ">>" shiftR
          ]

        , [ binaryOp "<"  (cmp (<))
          , binaryOp "<=" (cmp (<=))
          , binaryOp ">"  (cmp (>))
          , binaryOp ">=" (cmp (>=))
          ]

        , [ binaryOp "==" (cmp (==))
          , binaryOp "!=" (cmp (/=))
          ]

        , [ binaryOp "&" (.&.)
          , binaryOp "|" (.|.)
          , binaryOp "^" xor
          ]

        , [ binaryOp "&&" (bool (&&))
          , binaryOp "||" (bool (||))
          ]
        ]

    prefixOp s f = Prefix (op s >> return (ExUnary f))
    binaryOp s f = Infix (op s >> return (ExBinary f)) AssocLeft

    cmp f a b = fromEnum $ f a b
    bool f a b = cmp f (a /= 0) (b /= 0)

condExpr :: Parser Expr
condExpr = do
    e1 <- opExpr
    (ExConditional e1 <$> (op "?" >> expression) <*> (op ":" >> condExpr))
        <|> return e1

assignExpr :: Parser Expr
assignExpr =
    (try (flip ($) <$> tok variable <*> tok assignOps) <*> assignExpr)
    <|> condExpr
  where
    assignOps :: Parser (String -> Expr -> Expr)
    assignOps = choice
        [ op "=" >> return ExStore
        , assignOp "*=" (*)
        , assignOp "/=" quot
        , assignOp "%=" rem
        , assignOp "+=" (+)
        , assignOp "-=" (-)
        , assignOp "<<=" shiftL
        , assignOp ">>=" shiftR
        , assignOp "&=" (.&.)
        , assignOp "|=" (.|.)
        , assignOp "^=" xor
        ] <?> "assignment operator"
    assignOp s f = op s >> return (\v e -> ExStore v $ ExBinary f (ExFetch v) e)

op :: String -> Parser ()
op s = tok $ try (string s >> notFollowedBy (oneOf "-+~!*/<>=&|"))

variable :: Parser String
variable = name <?> "variable"

whitespace :: Parser ()
whitespace = skipMany $ oneOf " \t\n"

tok :: Parser a -> Parser a
tok a = a <* whitespace

