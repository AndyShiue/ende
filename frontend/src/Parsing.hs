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
import Text.Megaparsec.Perm
import qualified Text.Megaparsec.Expr as Expr
import qualified Text.Megaparsec.Lexer as Lexer

import Prelude hiding (mod)
    
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

literal :: Parser (Term Position)
literal = do
  start <- getWordPair
  int <- Lexer.integer
  end <- getWordPair
  space
  return . Literal (Position start end) . fromInteger $ int

identifier :: Parser String
identifier = lexeme $ some letterChar

var :: Parser (Term Position)
var = withPosition identifier >>= \(str, pos) -> return $ Var pos str

stringLiteral :: Parser (String, Position)
stringLiteral = do
  start <- getWordPair
  char '"'
  r <- manyTill Lexer.charLiteral $ char '"'
  end <- getWordPair
  space
  return $ (r, Position start end)
      
functionCall :: Parser (Term Position)
functionCall = do
  start <- getWordPair
  name <- withPosition identifier >>= \(str, pos) -> return $ FunctionCall pos str
  leftParen
  vars <- sepEndBy expr $ symbol ","
  (_, pair) <- rightParen
  let position = Position start (endPos pair)
  return $ Call position name vars

scope :: Parser (Term Position)
scope = do (b, pos) <- withPosition block
           return $ Scope pos b

if_clause :: Parser (Term Position)
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

while :: Parser (Term Position)
while = do
  start <- getWordPair
  symbol "while" <?> "while"
  t <- expr
  b <- block
  let pos = Position start (endPos $ getTag b)
  return $ While pos t b

term :: Parser (Term Position)
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
            Term Position ->
            Term Position ->
            Term Position
opToFunc op = let dummyPosition = Position (1, 1) (1, 1)
              in \l r -> Infix dummyPosition l op r

opToElement :: Operator -> Expr.Operator Parser (Term Position)
opToElement op = Expr.InfixL $ opToFunc op <$ (symbol $ opToString op)

table :: [[Expr.Operator Parser (Term Position)]]
table = [ [ opToElement Mul
          , opToElement Div ]
        , [ opToElement Add
          , opToElement Sub ] ]

expr :: Parser (Term Position)
expr = Expr.makeExprParser term table

semicolon :: Parser (String, Position)
semicolon = symbol ";" <?> "semicolon"

termSemicolon :: Parser (Statement Position)
termSemicolon = do
  start <- getWordPair
  t <- expr
  (_, scPos) <- semicolon
  let pos = Position start (endPos scPos)
  return $ TermSemicolon pos t

binding :: Parser (String, (Term Position))
binding = do
  var <- identifier <?> "variable name"
  symbol "=" <?> "equal sign"
  rhs <- expr
  return (var, rhs)

letBinding :: Parser (Statement Position)
letBinding = do
  start <- getWordPair
  symbol "let" <?> "let"
  (var, rhs) <- binding
  (_, scPos) <- semicolon
  let pos = Position start (endPos scPos)
  return $ Let pos var rhs

letMut :: Parser (Statement Position)
letMut = do
  start <- getWordPair
  symbol "let" <?> "let"
  symbol "mut" <?> "mut"
  (var, rhs) <- binding
  (_, scPos) <- semicolon
  let pos = Position start (endPos scPos)
  return $ LetMut pos var rhs

mutate :: Parser (Statement Position)
mutate = do
  start <- getWordPair
  (var, rhs) <- binding
  (_, scPos) <- semicolon
  let pos = Position start (endPos scPos)
  return $ Mutate pos var rhs




extern_stmt :: Parser (Statement Position)
extern_stmt = do
  start <- getWordPair
  symbol "extern" <?> "extern"
  fn <- identifier <?> "extern function name"
  args_ty <- ty
  (_, scPos) <- semicolon
  let pos = Position start (endPos scPos)
  return $ Extern pos fn args_ty

statement :: Parser (Statement Position)
statement =
  try letMut <|>
  extern_stmt <|>
  letBinding <|>
  try mutate <|>
  try termSemicolon <?> "statement"

block :: Parser (Block Position)
block = do
  start <- getWordPair
  symbol "{" <?> "left curly brace"
  stmts <- many statement
  end <- optional expr
  (_, endPosition) <- symbol "}" <?> "right curly brace"
  let pos = Position start (endPos endPosition)
  return $ Block pos stmts end

topLevelDecl :: Parser (TopLevelDecl Position)
topLevelDecl =
    try topLevelDecl' <|>
    try topLevelMod <|>
    try topLevelStmt

topLevelDecl' :: Parser (TopLevelDecl Position)
topLevelDecl' = do
  d <- decl
  return $ TopLevelDecl (getTag d) d

topLevelMod :: Parser (TopLevelDecl Position)
topLevelMod = do
  m <- mod
  return $ TopLevelMod (getTag m) m
         
topLevelStmt :: Parser (TopLevelDecl Position)
topLevelStmt = do
  s <- statement
  return $ TopLevelStmt (getTag s) s

langItem :: Parser (LangItem Position)
langItem = do
  start <- getWordPair
  symbol "@lang"
  leftParen
  (lit, pos) <- stringLiteral
  rightParen
  symbol ":"
  end <- getWordPair
  return $ LangItem (Position start end) lit
         
decl :: Parser (Decl Position)
decl =
    langItemDecl <|>
    dataDecl <|>
    funcDecl <|>
    recordDecl <|>
    implDecl

langItemDecl :: Parser (Decl Position)
langItemDecl = do
  start <- getWordPair
  li <- langItem
  d <- decl
  return $ LangItemDecl (Position start (endPos $ getTag d)) li d

dataDecl :: Parser (Decl Position)
dataDecl = do
  start <- getWordPair
  d <- data'
  return $ DataDecl (Position start (endPos $ getTag d)) d

funcDecl :: Parser (Decl Position)
funcDecl = do
  start <- getWordPair
  f <- function
  return $ FuncDecl (Position start (endPos $ getTag f)) f

recordDecl :: Parser (Decl Position)
recordDecl = do
  start <- getWordPair
  r <- record
  return $ RecordDecl (Position start (endPos $ getTag r)) r

implDecl :: Parser (Decl Position)
implDecl = do
  start <- getWordPair
  i <- impl
  return $ ImplDecl (Position start (endPos $ getTag i)) i

data' :: Parser (Data Position)
data' = do
  start <- getWordPair
  symbol "data"
  id <- identifier <?> "identifier"
  tl <- optional typeList
  dataNormal start id tl <|> dataGADT start id tl

dataNormal :: (Word, Word) -> String -> Maybe TypeList -> Parser (Data Position)
dataNormal start id tl = do
  symbol "="
  variants <- flip sepEndBy (symbol ",") $ do
                           start <- getWordPair
                           id <- identifier
                           end <- getWordPair
                           return $ Variant (Position start end) id []
  end <- getWordPair
  return $ Data (Position start end) id tl variants
dataGADT :: (Word, Word) -> String -> Maybe TypeList -> Parser (Data Position)
dataGADT  start id tl = do
  symbol "{"
  variants <- flip sepEndBy (symbol ",") $
                        gadtFnVariant <|>
                        gadtColonVariant

  symbol "}"
  end <- getWordPair
  return $ GADT (Position start end) id tl variants
gadtFnVariant :: Parser (GADTLikeVariant Position)
gadtFnVariant = do
  start <- getWordPair
  symbol "fn"
  id <- identifier
  tl <- typeList
  symbol "->"
  retTy <- ty
  end <- getWordPair
  return $ FuncVariant (Position start end) id tl retTy

gadtColonVariant :: Parser (GADTLikeVariant Position)
gadtColonVariant = do
  start <- getWordPair
  id <- identifier
  symbol ":"
  t <- ty
  end <- getWordPair
  return $ WithColonAnnotationVariant (Position start end) id t
  
translationUnit :: Parser (TranslationUnit Position)
translationUnit = do
  start <- getWordPair
  tld <- many topLevelDecl
  end <- getWordPair
  let pos = Position start end
  return $ TranslationUnit pos tld

function :: Parser (Function Position)
function = do
  start <- getWordPair
  pub <- fmap (\x -> case x of
                       Just _ -> Pub
                       Nothing -> NonPub) $ optional $ symbol "pub"
  const <- fmap (\x -> case x of
                         Just _ -> Const
                         Nothing -> NonConst) $ optional $ symbol "const"
  symbol "fn"
  id <- identifier
  tl <- typeList
  symbol "->"
  t <- ty
  functionNormal start pub const id tl t <|> functionPatternMatch start pub const id tl t

functionNormal :: (Word, Word) -> Visibility -> Constness -> String -> TypeList -> RetType -> Parser (Function Position)
functionNormal start vis con id tl ret = do
  symbol "="
  b <- block
  end <- getWordPair
  return $ Function (Position start end) vis con id tl ret b

branch :: Parser (Branch Position)
branch = do
  start <- getWordPair
  p <- pat
  symbol "=>"
  t <- term
  end <- getWordPair
  return $ Branch (Position start end) p t       

         
functionPatternMatch :: (Word, Word) -> Visibility -> Constness -> String -> TypeList -> RetType -> Parser (Function Position)
functionPatternMatch start vis con id tl ret = do
  symbol "{"
  p <- sepEndBy branch (symbol ",") 
  symbol "}"
  end <- getWordPair
  return $ FunctionPatternMatch (Position start end) vis con id tl ret p

record :: Parser (Record Position)
record = langItemRecord <|> record'

langItemRecord :: Parser (Record Position)
langItemRecord = do
  start <- getWordPair
  l <- langItem
  r <- record
  end <- getWordPair
  return $ LangItemRecord (Position start end) l r
         
record' :: Parser (Record Position)
record' = do
  start <- getWordPair
  pub <- fmap (\x -> case x of
                       Just _ -> Pub
                       Nothing -> NonPub) $ optional $ symbol "pub"
  symbol "record"
  id <- identifier
  tl <- typeList
  symbol "="
  constrName <- identifier
  variants <- flip sepEndBy (symbol ",") $
                        gadtFnVariant <|>
                        gadtColonVariant
  end <- getWordPair
  return $ Record (Position start end) pub id tl constrName variants

impl :: Parser (Impl Position)
impl = langItemImpl <|> implObj

langItemImpl :: Parser (Impl Position)
langItemImpl = do
  start <- getWordPair
  l <- langItem
  i <- impl
  end <- getWordPair
  return $ LangItemImpl (Position start end) l i

implObj :: Parser (Impl Position)
implObj = do
  start <- getWordPair
  pub <- fmap (\x -> case x of
                       Just _ -> Pub
                       Nothing -> NonPub) $ optional $ symbol "pub"
  symbol "impl"
  objName <- identifier
  symbol ":"
  recordName <- identifier
  tl <- typeList
  symbol "="
  constrName <- identifier
  symbol "{"
  b <- sepEndBy branch (symbol ",")
  symbol "}"
  symbol ";"
  end <- getWordPair
  return $ ImplObj (Position start end) pub objName recordName tl constrName b

mod :: Parser (Mod Position)
mod = langItemMod <|> mod'

langItemMod :: Parser (Mod Position)
langItemMod = do
  start <- getWordPair
  l <- langItem
  i <- mod
  end <- getWordPair
  return $ LangItemMod (Position start end) l i

mod' :: Parser (Mod Position)
mod' = do
  start <- getWordPair
  pub <- fmap (\x -> case x of
                       Just _ -> Pub
                       Nothing -> NonPub) $ optional $ symbol "pub"
  symbol "mod"
  id <- identifier
  symbol "{"
  decls <- many topLevelDecl
  symbol "}"
  end <- getWordPair
  return $ Mod (Position start end) pub id decls
-- TODO: finish pat and typeList and ty
pat :: Parser (Pat Position)
pat = undefined

typeList :: Parser TypeList
typeList = undefined

ty :: Parser Type
ty = undefined
           
-- TODO: Handle the error properly.
toTranslationUnit :: String -> TranslationUnit Position
toTranslationUnit str = unwrap $ parse translationUnit "" str
 where
   unwrap (Left err) = error $ show err
   unwrap (Right term) = term

parseTranslationUnit :: CString -> IO (StablePtr (TranslationUnit Position))
parseTranslationUnit x = do
  str <- peekCString x
  newStablePtr $!! toTranslationUnit str

foreign export ccall parseTranslationUnit :: CString -> IO (StablePtr (TranslationUnit Position))
