module Parsing
  ( block
  ) where

import Foreign.Ptr
import Foreign.StablePtr

import Data.Maybe
import Control.Monad (void)
import Control.DeepSeq (($!!))
import Text.Megaparsec hiding (space)
import Text.Megaparsec.String
import qualified Text.Megaparsec.Expr as Expr
import qualified Text.Megaparsec.Lexer as Lexer

import Ast

space :: Parser ()
space = Lexer.space (void spaceChar)
                    (Lexer.skipLineComment "--")
                    (Lexer.skipBlockCommentNested "{-" "-}")

symbol :: String -> Parser String
symbol = Lexer.symbol $ space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

leftParen :: Parser String
leftParen = symbol "(" <?> "left parenthesis"

rightParen :: Parser String
rightParen = symbol ")" <?> "right parenthesis"

literal :: Parser Term
literal = Literal . fromInteger <$> Lexer.integer

var :: Parser Term
var = Var <$> lexeme (some letterChar)

functionCall :: Parser Term
functionCall = do
    name <- lexeme $ some letterChar
    leftParen
    vars <- sepEndBy expr $ symbol ","
    rightParen
    let funcCall = FunctionCall name
    return $ Call funcCall vars

scope :: Parser Term
scope = Scope <$> block

if_clause :: Parser Term
if_clause = do
  symbol "if" <?> "if"
  cond <- expr
  symbol "then" <?> "then"
  then_part <- expr
  symbol "else" <?> "else"
  else_part <- expr
  return $ If cond then_part else_part

while :: Parser Term
while = do
  symbol "while" <?> "while"
  t <- expr
  b <- block
  return $ While t b

term :: Parser Term
term =
   (try if_clause <?> "if clause") <|>
   (try while <?> "while loop") <|>
   (try functionCall <?> "function call") <|>
   (try var <?> "variable") <|>
   (literal <?> "literal") <|>
   (scope <?> "scope")

opToString :: Operator -> String
opToString Add = "+"
opToString Sub = "-"
opToString Mul = "*"
opToString Div = "/"

opToFunc :: Operator -> Term -> Term -> Term
opToFunc op = \l r -> Infix l op r

opToElement :: Operator -> Expr.Operator Parser Term
opToElement op = Expr.InfixL $ opToFunc op <$ (symbol $ opToString op)

table = [ [ opToElement Mul
          , opToElement Div ]
        , [ opToElement Add
          , opToElement Sub ] ]

expr :: Parser Term
expr = Expr.makeExprParser term table

semicolon :: Parser String
semicolon = symbol ";" <?> "semicolon"

termSemicolon :: Parser Statement
termSemicolon = do
  t <- expr
  semicolon
  return $ TermSemicolon t

binding :: Parser (String, Term)
binding = do
  var <- lexeme (some letterChar) <?> "variable name"
  symbol "=" <?> "equal sign"
  rhs <- expr
  return (var, rhs)

letBinding :: Parser Statement
letBinding = do
  symbol "let" <?> "let"
  (var, rhs) <- binding
  semicolon
  return $ Let var rhs

letMut :: Parser Statement
letMut = do
  symbol "let" <?> "let"
  symbol "mut" <?> "mut"
  (var, rhs) <- binding
  semicolon
  return $ LetMut var rhs

mutate :: Parser Statement
mutate = do
  (var, rhs) <- binding
  semicolon
  return $ Mutate var rhs

-- TODO: This is a stub. Fix it.
ty :: Parser Type
ty = do
  symbol "("
  types <- symbol "I32" `sepEndBy` symbol ","
  symbol ")"
  symbol "->"
  symbol "I32"
  semicolon
  return $ FunctionTy (replicate (length types) I32Ty) I32Ty

extern_stmt :: Parser Statement
extern_stmt = do
  symbol "extern" <?> "extern"
  fn <- lexeme (some letterChar) <?> "extern function name"
  args_ty <- ty
  return $ Extern fn args_ty

statement :: Parser Statement
statement =
  try letMut <|>
  extern_stmt <|>
  letBinding <|>
  try mutate <|>
  try termSemicolon <?> "statement"

block :: Parser Block
block = do
  many spaceChar
  symbol "{" <?> "left curly brace"
  stmts <- many statement
  end <- optional expr
  many spaceChar
  symbol "}" <?> "right curly brace"
  return $ Block stmts end

program :: Parser Program
program = do
  symbol "fn" <?> "fn"
  symbol "main" <?> "main"
  leftParen
  rightParen
  symbol "->" <?> "arrow"
  symbol "Unit" <?> "Unit"
  b <- block
  return $ Program b

-- TODO: Handle the error properly.
toProgram :: String -> Program
toProgram str = unwrap $ parse program "" str
 where
   unwrap (Left err) = error $ show err
   unwrap (Right term) = term

program' :: Program
program' = toProgram "fn main() -> Unit { extern print(I32) -> I32; let mut countdown = 100; while countdown { print(countdown); countdown = countdown - 1; 0 }; 0 }"

getTree :: IO (StablePtr Program)
getTree = newStablePtr $!! program'

foreign export ccall getTree :: IO (StablePtr Program)
