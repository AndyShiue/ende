{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Ast ( Position(..)
           , Operator(..)
           , TaggedTerm(..)
           , TaggedFunctionCall(..)
           , TaggedStatement(..)
           , Type(..)
           , TaggedBlock(..)
           , TaggedProgram(..)
           , getTag
           ) where

import Control.DeepSeq
import GHC.Generics

data Position = Position { startPos :: (Word, Word)
                         , endPos :: (Word, Word)
                         } deriving (Show, Eq, Generic, NFData)

data Operator = Add | Sub | Mul | Div deriving (Show, Eq, Generic, NFData)

data TaggedTerm t = Literal t Int
                  | Var t String
                  | Infix t (TaggedTerm t) Operator (TaggedTerm t)
                  | Call t (TaggedFunctionCall t) [TaggedTerm t]
                  | Scope t (TaggedBlock t)
                  | If t (TaggedTerm t) (TaggedTerm t) (TaggedTerm t)
                  | While t (TaggedTerm t) (TaggedBlock t)
                  | Stmt (TaggedStatement t)
                  deriving (Show, Eq, Generic, NFData)

data TaggedFunctionCall t = TaggedFunctionCall t String deriving (Show, Eq, Generic, NFData)

data TaggedStatement t = TermSemicolon t (TaggedTerm t)
                       | Let t String (TaggedTerm t)
                       | LetMut t String (TaggedTerm t)
                       | Mutate t String (TaggedTerm t)
                       | Extern t String Type
                       deriving (Show, Eq, Generic, NFData)

data Type = Forbidden
          | I32Ty
          | FunctionTy [Type] Type
          deriving (Show, Eq, Generic, NFData)

data TaggedBlock t = TaggedBlock { tag :: t
                                 , stmts :: [TaggedStatement t]
                                 , end :: Maybe (TaggedTerm t)
                                 } deriving (Show, Eq, Generic, NFData)

data TaggedProgram t = TaggedProgram t (TaggedBlock t) deriving (Show, Eq, Generic, NFData)

-- TODO: the code below should be rewritten using GHC's Generic in th future.

class Tagged constr where
  getTag :: constr tag -> tag

instance Tagged TaggedTerm where
  getTag (Literal t _) = t
  getTag (Var t _) = t
  getTag (Ast.Infix t _ _ _) = t
  getTag (Call t _ _) = t
  getTag (Scope t _) = t
  getTag (If t _ _ _) = t
  getTag (While t _ _) = t
  getTag (Stmt stmt) = getTag stmt

instance Tagged TaggedFunctionCall where
  getTag (TaggedFunctionCall t _) = t

instance Tagged TaggedStatement where
  getTag (TermSemicolon t _) = t
  getTag (Let t _ _) = t
  getTag (LetMut t _ _) = t
  getTag (Mutate t _ _) = t
  getTag (Extern t _ _) = t

instance Tagged TaggedBlock where
  getTag block = tag block
