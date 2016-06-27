{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Ast ( Position(..)
           , Operator(..)
           , Term(..)
           , FunctionCall(..)
           , Statement(..)
           , Type(..)
           , Block(..)
           , TranslationUnit(..)
           , getTag
           ) where

import Control.DeepSeq
import GHC.Generics
type Params = [([Type], TypeMode)]
type FunctionName = String
type ImplObjName = String
type RecordName = String
type ConstrName = String
type ModName = String
data TypeMode = NormalMode
              | ConstMode
              | InstanceMode
              | PiMode
                deriving (Show, Eq, Generic, NFData)

data Position = Position { startPos :: (Word, Word)
                         , endPos :: (Word, Word)
                         } deriving (Show, Eq, Generic, NFData)

data Operator = Add | Sub | Mul | Div deriving (Show, Eq, Generic, NFData)

data Term t = Literal t Int
            | Var t String
            | Infix t (Term t) Operator (Term t)
            | Call t (FunctionCall t) [Term t]
            | Scope t (Block t)
            | If t (Term t) (Term t) (Term t)
            | While t (Term t) (Block t)
            | Stmt (Statement t)
            | Lam t 
              deriving (Show, Eq, Generic, NFData)

data FunctionCall t = FunctionCall t String deriving (Show, Eq, Generic, NFData)

data Function t = Function t FunctionName Params Type (Block t) deriving (Show, Eq, Generic, NFData)
data Lambda t = Lambda t Params Type (Block t) deriving (Show, Eq, Generic, NFData)
data Statement t = TermSemicolon t (Term t)
                 | Let t String (Term t)
                 | LetMut t String (Term t)
                 | Mutate t String (Term t)
                 | Extern t String Type
                   deriving (Show, Eq, Generic, NFData)

data Type = UnderScoreTy
          | VarTy String
          | WithColonTy Type Type
          | FunctionTy Params Type
          deriving (Show, Eq, Generic, NFData)

data Data t = Data t [Variant t]
            | GADT t [GADTLikeVariant t]
              deriving (Show, Eq, Generic, NFData)
data Variant t = Variant t String [Type] deriving (Show, Eq, Generic, NFData)
data Decl t = LangItemDecl t (LangItem t) (Decl t)
            | DataDecl t (Data t)
            | FuncDecl t (Function t)
            | RecordDecl t (Record t)
            | ImplDecl t (Impl t)
              deriving (Show, Eq, Generic, NFData)
data GADTLikeVariant t = WithColonAnnotationVariant String Type
                       | FuncVariant String Type
                         deriving (Show, Eq, Generic, NFData)
data Impl t = LangItemImpl t (LangItem t) (Impl t)
            | Impl t ImplObjName RecordName [Type] ConstrName [(String, Term t)]
              deriving (Show, Eq, Generic, NFData)
data Record t = LangItemRecord t (LangItem t) (Record t)
              | Record t RecordName [Type] ConstrName [GADTLikeVariant t]
                deriving (Show, Eq, Generic, NFData)

data Block t = LangItemBlock t (LangItem t) (Block t)
             | Block { tag :: t
                     , stmts :: [Statement t]
                     , end :: Maybe (Term t)
                     } deriving (Show, Eq, Generic, NFData)

data TopLevelDecl t = TopLevelDecl t (Decl t)
                    | TopLevelMod t (Mod t)
                    | TopLevelStmt t (Statement t)
                      deriving (Show, Eq, Generic, NFData)
data Mod t = LangItemMod t (LangItem t) (Mod t)
           | Mod t ModName [TopLevelDecl t] deriving (Show, Eq, Generic, NFData)
data LangItem t = LangItem t String deriving (Show, Eq, Generic, NFData)
data TranslationUnitAttr t = TranslationUnitAttr deriving (Show, Eq, Generic, NFData)

data TranslationUnit t = TranslationUnit t (TranslationUnitAttr t) [TopLevelDecl t] deriving (Show, Eq, Generic, NFData)

-- TODO: the code below should be rewritten using GHC's Generic in the future.

class Tagged constr where
  getTag :: constr tag -> tag

instance Tagged Term where
  getTag (Literal t _) = t
  getTag (Var t _) = t
  getTag (Ast.Infix t _ _ _) = t
  getTag (Call t _ _) = t
  getTag (Scope t _) = t
  getTag (If t _ _ _) = t
  getTag (While t _ _) = t
  getTag (Stmt stmt) = getTag stmt

instance Tagged FunctionCall where
  getTag (FunctionCall t _) = t

instance Tagged Statement where
  getTag (TermSemicolon t _) = t
  getTag (Let t _ _) = t
  getTag (LetMut t _ _) = t
  getTag (Mutate t _ _) = t
  getTag (Extern t _ _) = t

instance Tagged Block where
  getTag block = tag block
