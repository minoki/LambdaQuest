{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module LambdaQuest.SystemF.Type
  (TypeT(..,TyInt,TyReal,TyBool,TyUnit),Type
  ,TermT(..),Term
  ,isValue
  ,BindingT(..),Binding
  ,getTypeFromContext
  ,module LambdaQuest.Common.Type
  ) where
import LambdaQuest.Common.Type hiding (genPrimTypeOf)
import Data.Void

data TypeT a = TyPrim !PrimType
             | TyArr (TypeT a) (TypeT a)
             | TyRef !Int String         -- type variable (de Bruijn index)
             | TyAll String (TypeT a)    -- type abstraction (forall)
             | TyExtra !a
             deriving (Show,Functor,Foldable,Traversable)

pattern TyInt = TyPrim PTyInt
pattern TyReal = TyPrim PTyReal
pattern TyBool = TyPrim PTyBool
pattern TyUnit = TyPrim PTyUnit

data TermT a = TPrimValue !PrimValue             -- primitive value
             | TAbs String (TypeT a) (TermT a)   -- lambda abstraction
             | TTyAbs String (TermT a)           -- type abstraction
             | TRef !Int String                  -- variable (de Bruijn index)
             | TApp (TermT a) (TermT a)          -- function application
             | TTyApp (TermT a) (TypeT a)        -- type application
             | TLet String (TermT a) (TermT a)   -- let-in
             | TIf (TermT a) (TermT a) (TermT a) -- if-then-else
             deriving (Show,Functor,Foldable,Traversable)

data BindingT a = VarBind String (TypeT a) -- variable binding (name, type)
                | TyVarBind String    -- type variable binding (name)
                | AnonymousBind       -- placeholder for function type
                deriving (Eq,Show)

type Type = TypeT Void
type Term = TermT Void
type Binding = BindingT Void

isValue :: TermT a -> Bool
isValue t = case t of
  TPrimValue _ -> True
  TAbs _ _ _ -> True
  TTyAbs _ _ -> True
  TApp (TPrimValue (PVBuiltinBinary _)) x -> isValue x -- partial application
  _ -> False

instance (Eq a) => Eq (TypeT a) where
  TyPrim p  == TyPrim p'   = p == p'
  TyArr s t == TyArr s' t' = s == s' && t == t'
  TyRef i _ == TyRef i' _  = i == i'
  TyAll _ t == TyAll _ t'  = t == t' -- ignore type variable name
  TyExtra x == TyExtra x'  = x == x'
  _         == _           = False

instance (Eq a) => Eq (TermT a) where
  TPrimValue p == TPrimValue p' = p == p'
  TAbs _ t x   == TAbs _ t' x'  = t == t' && x == x' -- ignore variable name
  TTyAbs _ x   == TTyAbs _ x'   = x == x'            -- ignore type variable name
  TRef i _     == TRef i' _     = i == i'
  TApp s t     == TApp s' t'    = s == s' && t == t'
  TTyApp s t   == TTyApp s' t'  = s == s' && t == t'
  TLet _ s t   == TLet _ s' t'  = s == s' && t == t'
  TIf s t u    == TIf s' t' u'  = s == s' && t == t' && u == u'
  _            == _             = False

getTypeFromContext :: (Show a) => [BindingT a] -> Int -> TypeT a
getTypeFromContext ctx i
  | i < length ctx = case ctx !! i of
                       VarBind _ t -> t
                       b -> error ("TRef: expected a variable binding, found " ++ show b)
  | otherwise = error "TRef: index out of bounds"
