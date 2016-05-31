{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ast ( Operator(..)
           , Term(..)
           , FunctionCall(..)
           , Statement(..)
           , Block(..)
           ) where
import Control.DeepSeq
import GHC.Generics
data Operator = Add | Sub | Mul | Div deriving (Show, Eq, Generic, NFData)
data Term = Literal Int
          | Var String
          | Infix Term Operator Term
          | Call FunctionCall [Term]
          | Scope Block
          | If Term Term Term
          | While Term Block
          | Stmt Statement
          deriving (Show, Eq, Generic, NFData)
data FunctionCall = FunctionCall { name :: String
                                 , arity :: Int
                                 } deriving (Show, Eq, Generic, NFData)
data Statement = TermSemicolon Term
               | Let String Term
               | LetMut String Term
               | Mutate String Term
               | Extern String Int
               deriving (Show, Eq, Generic, NFData)
data Block = Block { stmts :: [Statement]
                   , end :: Term
                   } deriving (Show, Eq, Generic, NFData)
