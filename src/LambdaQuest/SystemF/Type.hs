{-# LANGUAGE PatternSynonyms #-}
module LambdaQuest.SystemF.Type
  (Type(..,TyInt,TyReal,TyBool,TyUnit)
  ,Term(..)
  ,isValue
  ,Binding(..)
  ,getTypeFromContext
  ,module LambdaQuest.Common.Type
  ) where
import LambdaQuest.Common.Type hiding (genPrimTypeOf)

data Type = TyPrim !PrimType
          | TyArr Type Type
          | TyRef !Int String     -- type variable (de Bruijn index)
          | TyAll String Type     -- type abstraction (forall)
          deriving (Show)

pattern TyInt = TyPrim PTyInt
pattern TyReal = TyPrim PTyReal
pattern TyBool = TyPrim PTyBool
pattern TyUnit = TyPrim PTyUnit

data Term = TPrimValue !PrimValue -- primitive value
          | TAbs String Type Term -- lambda abstraction
          | TTyAbs String Term    -- type abstraction
          | TRef !Int String      -- variable (de Bruijn index)
          | TApp Term Term        -- function application
          | TTyApp Term Type      -- type application
          | TLet String Term Term -- let-in
          | TIf Term Term Term    -- if-then-else
          deriving (Show)

data Binding = VarBind String Type -- variable binding (name, type)
             | TyVarBind String    -- type variable binding (name)
             | AnonymousBind       -- placeholder for function type
             deriving (Eq,Show)

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
  TyRef i _ == TyRef i' _  = i == i'
  TyAll _ t == TyAll _ t'  = t == t' -- ignore type variable name
  _         == _           = False

instance Eq Term where
  TPrimValue p == TPrimValue p' = p == p'
  TAbs _ t x   == TAbs _ t' x'  = t == t' && x == x' -- ignore variable name
  TTyAbs _ x   == TTyAbs _ x'   = x == x'            -- ignore type variable name
  TRef i _     == TRef i' _     = i == i'
  TApp s t     == TApp s' t'    = s == s' && t == t'
  TTyApp s t   == TTyApp s' t'  = s == s' && t == t'
  TLet _ s t   == TLet _ s' t'  = s == s' && t == t'
  TIf s t u    == TIf s' t' u'  = s == s' && t == t' && u == u'
  _            == _             = False

getTypeFromContext :: [Binding] -> Int -> Type
getTypeFromContext ctx i
  | i < length ctx = case ctx !! i of
                       VarBind _ t -> t
                       b -> error ("TRef: expected a variable binding, found " ++ show b)
  | otherwise = error "TRef: index out of bounds"
