{-# LANGUAGE PatternSynonyms #-}
module LambdaQuest.SystemF.Type where

data PrimType = PTyInt
              | PTyReal
              | PTyBool
              deriving (Eq,Show,Enum,Bounded)

data Type = TyPrim !PrimType
          | TyArr Type Type
          | TyRef !Int            -- type variable (de Brujin index)
          | TyAll String Type     -- type abstraction (forall)
          deriving (Show)

pattern TyInt = TyPrim PTyInt
pattern TyReal = TyPrim PTyReal
pattern TyBool = TyPrim PTyBool

data PrimValue = PVInt !Integer
               | PVReal !Double
               | PVBool !Bool
               deriving (Eq,Show)

data Value = VPrim !PrimValue      -- primitive value
           | VAbs String Type Term -- lambda abstraction
           | VTyAbs String Term    -- type abstraction
           deriving (Show)

pattern VInt x = VPrim (PVInt x)
pattern VReal x = VPrim (PVReal x)
pattern VBool x = VPrim (PVBool x)

data Term = TValue !Value
          | TRef !Int             -- variable (de Brujin index)
          | TApp Term Term        -- function application
          | TTyApp Term Type      -- type application
          | TIf Term Term Term    -- if-then-else
          deriving (Eq,Show)

pattern TAbs name ty body = TValue (VAbs name ty body)
pattern TTyAbs name body = TValue (VTyAbs name body)

instance Eq Type where
  TyPrim p  == TyPrim p'   = p == p'
  TyArr s t == TyArr s' t' = s == s' && t == t'
  TyRef i   == TyRef i'    = i == i'
  TyAll _ t == TyAll _ t'  = t == t'
  _         == _           = False

instance Eq Value where
  VPrim p    == VPrim p'     = p == p'
  VAbs _ t x == VAbs _ t' x' = t == t' && x == x'
  VTyAbs _ x == VTyAbs _ x'  = x == x'
  _          == _            = False
