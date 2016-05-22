module Ast ( Operator(..)
           , Term(..)
           , FunctionCall(..)
           , Statement(..)
           , Block(..)
           ) where

data Operator = Add | Sub | Mul | Div
data Term = Literal Integer
          | Var String
          | Infix Term Operator Term
          | Call FunctionCall [Term]
          | Scope Block
          | While Term Block
data FunctionCall = FunctionCall { name : String
                                 , arity : Int
                                 }
data Statement = TermSemicolon Term | Let String Term | LetMut String Term | Mutate String Term
data Block = Block { stmts : [Statement]
                   , end : Term
                   }
