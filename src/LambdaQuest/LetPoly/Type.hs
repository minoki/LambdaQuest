{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module LambdaQuest.LetPoly.Type
  (TypeT(..,TyInt,TyReal,TyBool,TyUnit),Type
  ,TypeSchemeT(..)
  ,TermT(..),Term
  ,isValue
  ,tyMapOnTerm
  ,BindingT(..),Binding
  ,getTypeSchemeFromContext
  ,module LambdaQuest.Common.Type
  ,GType(..)
  ) where
import LambdaQuest.Common.Type hiding (genPrimTypeOf)
import Data.Void

data TypeT a = TyPrim !PrimType
             | TyArr (TypeT a) (TypeT a)
             | TyRef !Int
             | TyExtra !a
             deriving (Show,Functor,Foldable,Traversable)

pattern TyInt = TyPrim PTyInt
pattern TyReal = TyPrim PTyReal
pattern TyBool = TyPrim PTyBool
pattern TyUnit = TyPrim PTyUnit

data TypeSchemeT a = Monotype (TypeT a)
                   | TyScAll (TypeSchemeT a)
                   deriving (Eq,Show,Functor,Foldable,Traversable)

data TermT a = TPrimValue !PrimValue             -- primitive value
             | TAbs String (TypeT a) (TermT a)   -- lambda abstraction
             | TRef !Int String                  -- variable (de Bruijn index)
             | TApp (TermT a) (TermT a)          -- function application
             | TLet String (TermT a) (TermT a)   -- let-in
             | TIf (TermT a) (TermT a) (TermT a) -- if-then-else
             -- | TTyAnn (TermT a) (TypeT a) -- type annotation
             deriving (Show,Functor,Foldable,Traversable)

data BindingT a = VarBind String (TypeSchemeT a) -- variable binding (name, type scheme)
                | TyVarBind
                | AnonymousBind                  -- placeholder for function type
                deriving (Eq,Show,Functor,Foldable,Traversable)

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
    TyRef i -> TyRef i
    TyExtra x -> fmap ($ x) s

instance Monad TypeT where
  return = TyExtra
  m >>= g = case m of
    TyPrim p -> TyPrim p
    TyArr u v -> TyArr (u >>= g) (v >>= g)
    TyRef i -> TyRef i
    TyExtra x -> g x

tyMapOnTermA :: (Applicative f) => (TypeT a -> f (TypeT b)) -> TermT a -> f (TermT b)
tyMapOnTermA f = g
  where
    g tm = case tm of
      TPrimValue p -> pure (TPrimValue p)
      TAbs name ty body -> TAbs name <$> f ty <*> g body
      TRef i name -> pure (TRef i name)
      TApp s t -> TApp <$> g s <*> g t
      TLet name def body -> TLet name <$> g def <*> g body
      TIf cond then_ else_ -> TIf <$> g cond <*> g then_ <*> g else_

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

tyMapOnTermD :: (Int -> TypeT a -> TypeT b) -> Int -> TermT a -> TermT b
tyMapOnTermD f = g
  where
    g !depth tm = case tm of
      TPrimValue p -> TPrimValue p
      TAbs name ty body -> TAbs name (f depth ty) (g (depth + 1) body)
      TRef i name -> TRef i name
      TApp s t -> TApp (g depth s) (g depth t)
      TLet name def body -> TLet name (g depth def) (g depth body)
      TIf cond then_ else_ -> TIf (g depth cond) (g depth then_) (g depth else_)

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
  TyRef i   == TyRef i'    = i == i'
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

getTypeSchemeFromContext :: (Show a) => [BindingT a] -> Int -> TypeSchemeT a
getTypeSchemeFromContext ctx i
  | i < length ctx = case ctx !! i of
                       VarBind _ t -> t
                       b -> error ("TRef: expected a variable binding, found " ++ show b)
  | otherwise = error "TRef: index out of bounds"
