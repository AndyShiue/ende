module Parsing
  ( block
  ) where

import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.String

import Data.Maybe
import Control.Monad (void)
import Control.DeepSeq (($!!))
import Text.Megaparsec hiding (space)
import Text.Megaparsec.String
import qualified Text.Megaparsec.Expr as Expr
import qualified Text.Megaparsec.Lexer as Lexer

import Ast

toTuple :: SourcePos -> (Word, Word)
toTuple pos = (unPos $ sourceLine pos, unPos $ sourceColumn pos)

getWordPair :: Parser (Word, Word)
getWordPair = toTuple <$> getPosition

space :: Parser ()
space = Lexer.space (void spaceChar)
                    (Lexer.skipLineComment "--")
                    (Lexer.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

withPosition :: Parser a -> Parser (a, Position)
withPosition parser = do start <- getWordPair
                         original <- parser
                         end <- getWordPair
                         return (original, Position start end)

symbol :: String -> Parser (String, Position)
symbol = lexeme . withPosition . string

leftParen :: Parser (String, Position)
leftParen = symbol "(" <?> "left parenthesis"

rightParen :: Parser (String, Position)
rightParen = symbol ")" <?> "right parenthesis"

literal :: Parser (TaggedTerm Position)
literal = do
  start <- getWordPair
  int <- Lexer.integer
  end <- getWordPair
  space
  return . Literal (Position start end) . fromInteger $ int

identifier :: Parser String
identifier = lexeme $ some letterChar

var :: Parser (TaggedTerm Position)
var = withPosition identifier >>= \(str, pos) -> return $ Var pos str

functionCall :: Parser (TaggedTerm Position)
functionCall = do
  start <- getWordPair
  name <- withPosition identifier >>= \(str, pos) -> return $ TaggedFunctionCall pos str
  leftParen
  vars <- sepEndBy expr $ symbol ","
  (_, pair) <- rightParen
  let position = Position start (endPos pair)
  return $ Call position name vars

scope :: Parser (TaggedTerm Position)
scope = do (b, pos) <- withPosition block
           return $ Scope pos b

if_clause :: Parser (TaggedTerm Position)
if_clause = do
  start <- getWordPair
  symbol "if" <?> "if"
  cond <- expr
  symbol "then" <?> "then"
  thenPart <- expr
  symbol "else" <?> "else"
  elsePart <- expr
  let pos = Position start (endPos $ getTag elsePart)
  return $ If pos cond thenPart elsePart

while :: Parser (TaggedTerm Position)
while = do
  start <- getWordPair
  symbol "while" <?> "while"
  t <- expr
  b <- block
  let pos = Position start (endPos $ getTag b)
  return $ While pos t b

term :: Parser (TaggedTerm Position)
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

-- TODO: Now only a dummy position is being tagged. Fix it.
opToFunc :: Operator ->
            TaggedTerm Position ->
            TaggedTerm Position ->
            TaggedTerm Position
opToFunc op = let dummyPosition = Position (1, 1) (1, 1)
              in \l r -> Infix dummyPosition l op r

opToElement :: Operator -> Expr.Operator Parser (TaggedTerm Position)
opToElement op = Expr.InfixL $ opToFunc op <$ (symbol $ opToString op)

table :: [[Expr.Operator Parser (TaggedTerm Position)]]
table = [ [ opToElement Mul
          , opToElement Div ]
        , [ opToElement Add
          , opToElement Sub ] ]

expr :: Parser (TaggedTerm Position)
expr = Expr.makeExprParser term table

semicolon :: Parser (String, Position)
semicolon = symbol ";" <?> "semicolon"

termSemicolon :: Parser (TaggedStatement Position)
termSemicolon = do
  start <- getWordPair
  t <- expr
  (_, scPos) <- semicolon
  let pos = Position start (endPos scPos)
  return $ TermSemicolon pos t

binding :: Parser (String, (TaggedTerm Position))
binding = do
  var <- identifier <?> "variable name"
  symbol "=" <?> "equal sign"
  rhs <- expr
  return (var, rhs)

letBinding :: Parser (TaggedStatement Position)
letBinding = do
  start <- getWordPair
  symbol "let" <?> "let"
  (var, rhs) <- binding
  (_, scPos) <- semicolon
  let pos = Position start (endPos scPos)
  return $ Let pos var rhs

letMut :: Parser (TaggedStatement Position)
letMut = do
  start <- getWordPair
  symbol "let" <?> "let"
  symbol "mut" <?> "mut"
  (var, rhs) <- binding
  (_, scPos) <- semicolon
  let pos = Position start (endPos scPos)
  return $ LetMut pos var rhs

mutate :: Parser (TaggedStatement Position)
mutate = do
  start <- getWordPair
  (var, rhs) <- binding
  (_, scPos) <- semicolon
  let pos = Position start (endPos scPos)
  return $ Mutate pos var rhs

-- TODO: This is a stub. Fix it.
ty :: Parser Type
ty = do
  symbol "("
  types <- symbol "I32" `sepEndBy` symbol ","
  symbol ")"
  symbol "->"
  symbol "I32"
  return $ FunctionTy (replicate (length types) I32Ty) I32Ty

extern_stmt :: Parser (TaggedStatement Position)
extern_stmt = do
  start <- getWordPair
  symbol "extern" <?> "extern"
  fn <- identifier <?> "extern function name"
  args_ty <- ty
  (_, scPos) <- semicolon
  let pos = Position start (endPos scPos)
  return $ Extern pos fn args_ty

statement :: Parser (TaggedStatement Position)
statement =
  try letMut <|>
  extern_stmt <|>
  letBinding <|>
  try mutate <|>
  try termSemicolon <?> "statement"

block :: Parser (TaggedBlock Position)
block = do
  start <- getWordPair
  symbol "{" <?> "left curly brace"
  stmts <- many statement
  end <- optional (expr <|> (Stmt <$> statement))
  (_, endPosition) <- symbol "}" <?> "right curly brace"
  let pos = Position start (endPos endPosition)
  return $ TaggedBlock pos stmts end

program :: Parser (TaggedProgram Position)
program = do
  start <- getWordPair
  symbol "fn" <?> "fn"
  symbol "main" <?> "main"
  leftParen
  rightParen
  symbol "->" <?> "arrow"
  symbol "Unit" <?> "Unit"
  b <- block
  (_, scPos) <- semicolon
  let pos = Position start (endPos scPos)
  return $ TaggedProgram pos b

-- TODO: Handle the error properly.
toProgram :: String -> TaggedProgram Position
toProgram str = unwrap $ parse program "" str
 where
   unwrap (Left err) = error $ show err
   unwrap (Right term) = term

program' :: TaggedProgram Position
program' = toProgram "fn main() -> Unit { extern print(I32) -> I32; let mut countdown = 100; while countdown { print(countdown); countdown = countdown - 1; 0 }; 0 };"

parseProgram :: CString -> IO (StablePtr (TaggedProgram Position))
parseProgram x = do
  str <- peekCString x
  newStablePtr $!! toProgram str

foreign export ccall parseProgram :: CString -> IO (StablePtr (TaggedProgram Position))
