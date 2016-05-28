module Ast ( Operator(..)
           , Term(..)
           , FunctionCall(..)
           , Statement(..)
           , Block(..)
           ) where

data Operator = Add | Sub | Mul | Div deriving (Show, Eq)
data Term = Literal Integer
          | Var String
          | Infix Term Operator Term
          | Call FunctionCall [Term]
          | Scope Block
          | If Term Term Term
          | While Term Block
          | Stmt Statement
          deriving (Show, Eq)
data FunctionCall = FunctionCall { name :: String
                                 , arity :: Int
                                 } deriving (Show, Eq)
data Statement = TermSemicolon Term
               | Let String Term
               | LetMut String Term
               | Mutate String Term
               | Extern String Int
               deriving (Show, Eq)
data Block = Block { stmts :: [Statement]
                   , end :: Term
                   } deriving (Show, Eq)
