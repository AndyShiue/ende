module Ast ( Operator(..)
           , Term(..)
           , FunctionCall(..)
           , Statement(..)
           , Block(..)
           ) where
import Foreign.Ptr
import Foreign.StablePtr

data Operator = Add | Sub | Mul | Div deriving (Show, Eq)
data Term = Literal Integer
          | Var String
          | Infix Term Operator Term
          | Call FunctionCall [Term]
          | Scope Block
          | While Term Block
          deriving (Show, Eq)
data FunctionCall = FunctionCall { name :: String
                                 , arity :: Int
                                 } deriving (Show, Eq)
data Statement = TermSemicolon Term
               | Let String Term
               | LetMut String Term
               | Mutate String Term
               deriving (Show, Eq)
data Block = Block { stmts :: [Statement]
                   , end :: Term
                   } deriving (Show, Eq)
