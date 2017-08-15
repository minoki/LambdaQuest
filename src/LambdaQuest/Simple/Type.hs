{-# LANGUAGE PatternSynonyms #-}
module LambdaQuest.Simple.Type
  (Type(..,TyInt,TyReal,TyBool,TyUnit)
  ,Term(..)
  ,isValue
  ,Binding(..)
  ,getTypeFromContext
  ,module LambdaQuest.Common.Type
  ,GType(..)
  ) where
import LambdaQuest.Common.Type hiding (genPrimTypeOf)

data Type = TyPrim !PrimType
          | TyArr Type Type
          deriving (Show)

pattern TyInt = TyPrim PTyInt
pattern TyReal = TyPrim PTyReal
pattern TyBool = TyPrim PTyBool
pattern TyUnit = TyPrim PTyUnit

data Term = TPrimValue !PrimValue -- primitive value
          | TAbs String Type Term -- lambda abstraction
          | TRef !Int String      -- variable (de Bruijn index)
          | TApp Term Term        -- function application
          | TIf Term Term Term    -- if-then-else
          deriving (Show)

data Binding = VarBind String Type -- variable binding (name, type)
             | AnonymousBind       -- placeholder for function type
             deriving (Eq,Show)

isValue :: Term -> Bool
isValue t = case t of
  TPrimValue _ -> True
  TAbs _ _ _ -> True
  TApp (TPrimValue (PVBuiltinBinary _)) x -> isValue x -- partial application
  _ -> False

class GType ty where
  tyPrim :: PrimType -> ty
  tyArr :: ty -> ty -> ty
  tyUnit, tyInt, tyReal, tyBool :: ty
  tyUnit = tyPrim PTyUnit
  tyInt = tyPrim PTyInt
  tyReal = tyPrim PTyReal
  tyBool = tyPrim PTyBool

instance GType Type where
  tyPrim = TyPrim
  tyArr = TyArr

instance Eq Type where
  TyPrim p  == TyPrim p'   = p == p'
  TyArr s t == TyArr s' t' = s == s' && t == t'
  _         == _           = False

instance Eq Term where
  TPrimValue p == TPrimValue p' = p == p'
  TAbs _ t x   == TAbs _ t' x'  = t == t' && x == x' -- ignore variable name
  TRef i _     == TRef i' _     = i == i'
  TApp s t     == TApp s' t'    = s == s' && t == t'
  TIf s t u    == TIf s' t' u'  = s == s' && t == t' && u == u'
  _            == _             = False

getTypeFromContext :: [Binding] -> Int -> Type
getTypeFromContext ctx i
  | i < length ctx = case ctx !! i of
                       VarBind _ t -> t
                       b -> error ("TRef: expected a variable binding, found " ++ show b)
  | otherwise = error "TRef: index out of bounds"
