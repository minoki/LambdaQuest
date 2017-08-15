{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
module LambdaQuest.Finter.Type
  (Type(..,TyInt,TyReal,TyBool,TyUnit)
  ,CCanonicalType
  ,ICanonicalType(..)
  ,BuiltinUnaryFn(..)
  ,BuiltinBinaryFn(..)
  ,PrimValue
  ,primTypeOf
  ,Term(..)
  ,Binding(..)
  ,isValue
  ,getTypeFromContext
  ,getBoundFromContext
  ,getBoundFromCContext
  ,canonicalToOrdinary
  ,iCanonicalToOrdinary
  ,typeShift
  ,typeSubstD
  ,typeSubst
  ,typeShiftC
  ,typeShiftI
  ,module LambdaQuest.Common.Type
  ) where
import LambdaQuest.Common.Type hiding (genPrimTypeOf,PrimValue,BuiltinUnaryFn,BuiltinBinaryFn)
import qualified LambdaQuest.Common.Type as CT

data Type = TyPrim !PrimType
          | TyArr Type Type
          | TyRef !Int String       -- type variable (de Bruijn index)
          | TyAll String Type Type  -- bounded type abstraction (forall)
          | TyInter [Type]          -- intersection type
          | TyTop
          deriving (Show)

pattern TyInt = TyPrim PTyInt
pattern TyReal = TyPrim PTyReal
pattern TyBool = TyPrim PTyBool
pattern TyUnit = TyPrim PTyUnit

-- compound canonical type
type CCanonicalType = [ICanonicalType]

-- individual canonical type
data ICanonicalType = CTyPrim !PrimType
                    | CTyArr CCanonicalType ICanonicalType
                    | CTyRef !Int String
                    | CTyAll String CCanonicalType ICanonicalType
                    deriving (Show)

data BuiltinUnaryFn = BUnaryCommon !CT.BuiltinUnaryFn
                    | BNegate
                    deriving (Eq,Show)

data BuiltinBinaryFn = BBinaryCommon !CT.BuiltinBinaryFn
                     | BAdd
                     | BSub
                     | BMul
                     deriving (Eq,Show)

type PrimValue = CT.PrimValueT BuiltinUnaryFn BuiltinBinaryFn

primTypeOf :: PrimValue -> Type
primTypeOf = genPrimTypeOfT builtinUnaryFnType builtinBinaryFnType TyPrim TyArr
  where
    builtinUnaryFnType c f = case f of
      BUnaryCommon f -> c f
      BNegate -> TyInter [TyArr TyInt TyInt
                         ,TyArr TyReal TyReal
                         ]
    builtinBinaryFnType c f = case f of
      BBinaryCommon f -> c f
      BAdd -> TyInter [TyArr TyInt (TyArr TyInt TyInt)
                      ,TyArr TyReal (TyArr TyReal TyReal)
                      ]
      BSub -> TyInter [TyArr TyInt (TyArr TyInt TyInt)
                      ,TyArr TyReal (TyArr TyReal TyReal)
                      ]
      BMul -> TyInter [TyArr TyInt (TyArr TyInt TyInt)
                      ,TyArr TyReal (TyArr TyReal TyReal)
                      ]

data Term = TPrimValue !PrimValue   -- primitive value
          | TAbs String Type Term   -- lambda abstraction
          | TTyAbs String Type Term -- bounded type abstraction
          | TRef !Int String        -- variable (de Bruijn index)
          | TApp Term Term          -- function application
          | TTyApp Term Type        -- type application
          | TLet String Term Term   -- let-in
          | TIf Term Term Term      -- if-then-else
          | TCoerce Term Type       -- type coercion
          | TFor String [Type] Term -- for <tyvar> in ...
          deriving (Show)

data Binding = VarBind String CCanonicalType   -- variable binding (name, type)
             | TyVarBind String CCanonicalType -- type variable binding (name, upper bound)
             | AnonymousBind                   -- placeholder for function type
             deriving (Eq,Show)

isValue :: Term -> Bool
isValue t = case t of
  TPrimValue _ -> True
  TAbs _ _ _ -> True
  TTyAbs _ _ _ -> True
  TApp (TPrimValue (PVBuiltinBinary _)) x -> isValue x -- partial application
  _ -> False

instance Eq Type where
  TyPrim p    == TyPrim p'     = p == p'
  TyArr s t   == TyArr s' t'   = s == s' && t == t'
  TyRef i _   == TyRef i' _    = i == i'
  TyAll _ b t == TyAll _ b' t' = b == b' && t == t' -- ignore type variable name
  TyInter tys == TyInter tys'  = tys == tys'
  TyTop       == TyTop         = True
  _           == _             = False

instance Eq ICanonicalType where
  CTyPrim p    == CTyPrim p'     = p == p'
  CTyArr s t   == CTyArr s' t'   = s == s' && t == t'
  CTyRef i _   == CTyRef i' _    = i == i'
  CTyAll _ b t == CTyAll _ b' t' = b == b' && t == t'
  _            == _              = False

instance Eq Term where
  TPrimValue p == TPrimValue p'  = p == p'
  TAbs _ t x   == TAbs _ t' x'   = t == t' && x == x' -- ignore variable name
  TTyAbs _ b x == TTyAbs _ b' x' = x == x'            -- ignore type variable name
  TRef i _     == TRef i' _      = i == i'
  TApp s t     == TApp s' t'     = s == s' && t == t'
  TTyApp s t   == TTyApp s' t'   = s == s' && t == t'
  TLet _ s t   == TLet _ s' t'   = s == s' && t == t'
  TIf s t u    == TIf s' t' u'   = s == s' && t == t' && u == u'
  TCoerce x t  == TCoerce x' t'  = x == x' && t == t'
  TFor _ tys x == TFor _ tys' x' = tys == tys && x == x'
  _            == _              = False

-- replaces occurrences of TyRef j (j >= i) with TyRef (j + delta)
typeShift :: Int -> Int -> Type -> Type
typeShift delta i t = case t of
  TyPrim _ -> t
  TyTop -> t
  TyArr u v -> TyArr (typeShift delta i u) (typeShift delta (i + 1) v)
  TyRef j name | j >= i, j + delta >= 0 -> TyRef (j + delta) name
               | j >= i, j + delta < 0 -> error "typeShift: negative index"
               | otherwise -> t
  TyAll n b t -> TyAll n (typeShift delta i b) (typeShift delta (i + 1) t)
  TyInter tys -> TyInter (map (typeShift delta i) tys)
-- typeShift 0 i t == t
-- typeShift d1 i (typeShift d0 i t) == typeShift (d1 + d0) i t

typeSubstD :: Int -> Type -> Int -> Type -> Type
typeSubstD depth s i t = case t of
  TyPrim _ -> t
  TyTop -> t
  TyArr u v -> TyArr (typeSubstD depth s i u) (typeSubstD (depth + 1) s (i + 1) v)
  TyRef j name | j == i -> typeShift depth 0 s
               | j > i -> TyRef (j - 1) name
               | otherwise -> t
  TyAll n b t -> TyAll n (typeSubstD depth s i b) (typeSubstD (depth + 1) s (i + 1) t)
  TyInter tys -> TyInter (map (typeSubstD depth s i) tys)

-- replaces occurrences of TyRef j (j > i) with TyRef (j-1), and TyRef i with the given type
typeSubst = typeSubstD 0

getTypeFromContext :: [Binding] -> Int -> Type
getTypeFromContext ctx i
  | i < length ctx = case ctx !! i of
                       VarBind _ t -> canonicalToOrdinary t
                       b -> error ("TRef: expected a variable binding, found " ++ show b)
  | otherwise = error "TRef: index out of bounds"

getBoundFromContext :: [Binding] -> Int -> Type
getBoundFromContext ctx i
  | i < length ctx = case ctx !! i of
                       TyVarBind _ b -> canonicalToOrdinary b
                       b -> error ("TyRef: expected a type variable binding, found " ++ show b)
  | otherwise = error "TyRef: index out of bounds"

getBoundFromCContext :: [Binding] -> Int -> CCanonicalType
getBoundFromCContext ctx i
  | i < length ctx = case ctx !! i of
                       TyVarBind _ b -> b
                       b -> error ("TyRef: expected a type variable binding, found " ++ show b)
  | otherwise = error "TyRef: index out of bounds"

canonicalToOrdinary :: CCanonicalType -> Type
canonicalToOrdinary [] = TyTop -- top
canonicalToOrdinary [t] = iCanonicalToOrdinary t
canonicalToOrdinary tys = TyInter $ map iCanonicalToOrdinary tys

iCanonicalToOrdinary :: ICanonicalType -> Type
iCanonicalToOrdinary (CTyPrim p) = TyPrim p
iCanonicalToOrdinary (CTyArr s t) = TyArr (canonicalToOrdinary s) (iCanonicalToOrdinary t)
iCanonicalToOrdinary (CTyRef i name) = TyRef i name
iCanonicalToOrdinary (CTyAll name b t) = TyAll name (canonicalToOrdinary b) (iCanonicalToOrdinary t)

typeShiftC :: Int -> Int -> CCanonicalType -> CCanonicalType
typeShiftC delta i = map (typeShiftI delta i)

typeShiftI :: Int -> Int -> ICanonicalType -> ICanonicalType
typeShiftI delta i t = case t of
  CTyPrim _ -> t
  CTyArr s t -> CTyArr (typeShiftC delta i s) (typeShiftI delta i t)
  CTyRef j name | j >= i, j + delta >= 0 -> CTyRef (j + delta) name
                | j >= i, j + delta < 0 -> error "typeShift: negative index"
                | otherwise -> t
  CTyAll name b t -> CTyAll name (typeShiftC delta i b) (typeShiftI delta (i + 1) t)
