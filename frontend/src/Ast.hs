{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ast ( Operator(..)
           , Term(..)
           , FunctionCall(..)
           , Statement(..)
           , Type(..)
           , Block(..)
           , Program(..)
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
                                 } deriving (Show, Eq, Generic, NFData)
data Statement = TermSemicolon Term
               | Let String Term
               | LetMut String Term
               | Mutate String Term
               | Extern String Type
               deriving (Show, Eq, Generic, NFData)
data Type = Forbidden
          | I32Ty
          | FunctionTy [Type] Type
          deriving (Show, Eq, Generic, NFData)
data Block = Block { stmts :: [Statement]
                   , end :: Maybe Term
                   } deriving (Show, Eq, Generic, NFData)
data Program = Program Block deriving (Show, Eq, Generic, NFData)
