{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DefaultSignatures, TypeOperators, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, KindSignatures, MultiParamTypeClasses, OverlappingInstances, IncoherentInstances #-}

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
import Debug.Trace
type Param = Type
type ParamList = [([Param], TypeMode)]
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
            | Lam t (Lambda t)
              deriving (Show, Eq, Generic, NFData)

data FunctionCall t = FunctionCall t String deriving (Show, Eq, Generic, NFData)

data Function t = Function t FunctionName ParamList Type (Block t) deriving (Show, Eq, Generic, NFData)
data Lambda t = Lambda t ParamList Type (Block t) deriving (Show, Eq, Generic, NFData)
data Statement t = TermSemicolon t (Term t)
                 | Let t String (Term t)
                 | LetMut t String (Term t)
                 | Mutate t String (Term t)
                 | Extern t String Type
                   deriving (Show, Eq, Generic, NFData)

data Type = UnderScoreTy
          | VarTy String
          | WithColonTy Type Type
          | FunctionTy ParamList Type
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
data GADTLikeVariant t = WithColonAnnotationVariant t String Type
                       | FuncVariant t String Type
                         deriving (Show, Eq, Generic, NFData)
data Impl t = LangItemImpl t (LangItem t) (Impl t)
            | Impl t ImplObjName RecordName [Type] ConstrName [(String, Term t)]
              deriving (Show, Eq, Generic, NFData)
data Record t = LangItemRecord t (LangItem t) (Record t)
              | Record t RecordName [Type] ConstrName [GADTLikeVariant t]
                deriving (Show, Eq, Generic, NFData)

data Block t = LangItemBlock t (LangItem t) (Block t)
             | Block t [Statement t] (Maybe (Term t))
               deriving (Show, Eq, Generic, NFData)

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

class GTagged (tag :: *) (f :: * -> *) where
    gGetTag :: f p -> tag


instance (GTagged tag a, GTagged tag b) => GTagged tag (a :*: b) where
    gGetTag (a :*: b) = gGetTag a

instance (GTagged tag a, GTagged tag b) => GTagged tag (a :+: b) where
    gGetTag (L1 x) = gGetTag x
    gGetTag (R1 x) = gGetTag x

instance (GTagged tag a) => GTagged tag (D1 c a) where
    gGetTag (M1 x) = gGetTag x

instance GTagged tag a => GTagged tag (C1 c a) where
    gGetTag (M1 x) = gGetTag x

instance GTagged tag a => GTagged tag (S1 c a) where
    gGetTag (M1 x) = gGetTag x

instance GTagged tag (K1 r tag) where
    gGetTag (K1 x) = x
instance GTagged tag (K1 r tag1) where
    gGetTag = undefined

class Tagged constr where
  getTag :: Show tag => constr tag -> tag
  default getTag :: (Generic (constr tag), GTagged tag (Rep (constr tag))) => constr tag -> tag
  getTag x = gGetTag $ from x

instance Tagged Term
instance Tagged FunctionCall
instance Tagged Statement
instance Tagged Block

