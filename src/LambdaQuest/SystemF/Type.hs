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

data BuiltinUnaryFn = BNegateInt
                    | BNegateReal
                    | BIntToReal
                    deriving (Eq,Show,Enum,Bounded)

data BuiltinBinaryFn = BAddInt
                     | BSubInt
                     | BMulInt
                     | BLtInt
                     | BLeInt
                     | BEqualInt
                     | BAddReal
                     | BSubReal
                     | BMulReal
                     | BDivReal
                     | BLtReal
                     | BLeReal
                     | BEqualReal
                     deriving (Eq,Show,Enum,Bounded)

data PrimValue = PVInt !Integer
               | PVReal !Double
               | PVBool !Bool
               | PVBuiltinUnary !BuiltinUnaryFn
               | PVBuiltinBinary !BuiltinBinaryFn
               deriving (Eq,Show)

data Term = TPrimValue !PrimValue -- primitive value
          | TAbs String Type Term -- lambda abstraction
          | TTyAbs String Term    -- type abstraction
          | TRef !Int             -- variable (de Brujin index)
          | TApp Term Term        -- function application
          | TTyApp Term Type      -- type application
          | TIf Term Term Term    -- if-then-else
          deriving (Show)

isValue :: Term -> Bool
isValue t = case t of
  TPrimValue _ -> True
  TAbs _ _ _ -> True
  TTyAbs _ _ -> True
  TApp (TPrimValue (PVBuiltinBinary _)) x -> isValue x -- partial application
  _ -> False

instance Eq Type where
  TyPrim p  == TyPrim p'   = p == p'
  TyArr s t == TyArr s' t' = s == s' && t == t'
  TyRef i   == TyRef i'    = i == i'
  TyAll _ t == TyAll _ t'  = t == t' -- ignore type variable name
  _         == _           = False

instance Eq Term where
  TPrimValue p == TPrimValue p' = p == p'
  TAbs _ t x   == TAbs _ t' x'  = t == t' && x == x' -- ignore variable name
  TTyAbs _ x   == TTyAbs _ x'   = x == x'            -- ignore type variable name
  TRef i       == TRef i'       = i == i'
  TApp s t     == TApp s' t'    = s == s' && t == t'
  TTyApp s t   == TTyApp s' t'  = s == s' && t == t'
  TIf s t u    == TIf s' t' u'  = s == s' && t == t' && u == u'
  _            == _             = False
