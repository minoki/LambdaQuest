{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module LambdaQuest.Simple.Type
  (TypeT(..,TyInt,TyReal,TyBool,TyUnit),Type
  ,TermT(..),Term
  ,isValue
  ,tyMapOnTerm
  ,BindingT(..),Binding
  ,getTypeFromContext
  ,module LambdaQuest.Common.Type
  ,GType(..)
  ) where
import LambdaQuest.Common.Type hiding (genPrimTypeOf)
import Data.Void

data TypeT a = TyPrim !PrimType
             | TyArr (TypeT a) (TypeT a)
             | TyExtra !a
             deriving (Show,Functor,Foldable,Traversable)

pattern TyInt = TyPrim PTyInt
pattern TyReal = TyPrim PTyReal
pattern TyBool = TyPrim PTyBool
pattern TyUnit = TyPrim PTyUnit

data TermT a = TPrimValue !PrimValue             -- primitive value
             | TAbs String (TypeT a) (TermT a)   -- lambda abstraction
             | TRef !Int String                  -- variable (de Bruijn index)
             | TApp (TermT a) (TermT a)          -- function application
             | TLet String (TermT a) (TermT a)   -- let-in
             | TIf (TermT a) (TermT a) (TermT a) -- if-then-else
             -- | TTyAnn (TermT a) (TypeT a) -- type annotation
             deriving (Show,Functor,Foldable,Traversable)

data BindingT a = VarBind String (TypeT a) -- variable binding (name, type)
                | AnonymousBind            -- placeholder for function type
                deriving (Eq,Show)

type Type = TypeT Void
type Term = TermT Void
type Binding = BindingT Void

isValue :: TermT a -> Bool
isValue t = case t of
  TPrimValue _ -> True
  TAbs _ _ _ -> True
  TApp (TPrimValue (PVBuiltinBinary _)) x -> isValue x -- partial application
  _ -> False

instance Applicative TypeT where
  pure = TyExtra
  TyExtra f <*> t = fmap f t
  s <*> t = case t of
    TyPrim p -> TyPrim p
    TyArr u v -> TyArr (s <*> u) (s <*> v)
    TyExtra x -> fmap ($ x) s

instance Monad TypeT where
  return = TyExtra
  m >>= g = case m of
    TyPrim p -> TyPrim p
    TyArr u v -> TyArr (u >>= g) (v >>= g)
    TyExtra x -> g x

tyMapOnTerm :: (TypeT a -> TypeT b) -> TermT a -> TermT b
tyMapOnTerm f = g
  where
    g tm = case tm of
      TPrimValue p -> TPrimValue p
      TAbs name ty body -> TAbs name (f ty) (g body)
      TRef i name -> TRef i name
      TApp s t -> TApp (g s) (g t)
      TLet name def body -> TLet name (g def) (g body)
      TIf cond then_ else_ -> TIf (g cond) (g then_) (g else_)

class GType ty where
  tyPrim :: PrimType -> ty
  tyArr :: ty -> ty -> ty
  tyUnit, tyInt, tyReal, tyBool :: ty
  tyUnit = tyPrim PTyUnit
  tyInt = tyPrim PTyInt
  tyReal = tyPrim PTyReal
  tyBool = tyPrim PTyBool

instance GType (TypeT a) where
  tyPrim = TyPrim
  tyArr = TyArr

instance (Eq a) => Eq (TypeT a) where
  TyPrim p  == TyPrim p'   = p == p'
  TyArr s t == TyArr s' t' = s == s' && t == t'
  TyExtra x == TyExtra x'  = x == x'
  _         == _           = False

instance (Eq a) => Eq (TermT a) where
  TPrimValue p == TPrimValue p' = p == p'
  TAbs _ t x   == TAbs _ t' x'  = t == t' && x == x' -- ignore variable name
  TRef i _     == TRef i' _     = i == i'
  TApp s t     == TApp s' t'    = s == s' && t == t'
  TLet _ s t   == TLet _ s' t'  = s == s' && t == t'
  TIf s t u    == TIf s' t' u'  = s == s' && t == t' && u == u'
  _            == _             = False

getTypeFromContext :: (Show a) => [BindingT a] -> Int -> TypeT a
getTypeFromContext ctx i
  | i < length ctx = case ctx !! i of
                       VarBind _ t -> t
                       b -> error ("TRef: expected a variable binding, found " ++ show b)
  | otherwise = error "TRef: index out of bounds"
