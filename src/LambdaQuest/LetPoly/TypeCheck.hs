{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module LambdaQuest.LetPoly.TypeCheck where
import LambdaQuest.LetPoly.Type
import LambdaQuest.LetPoly.Parse
import LambdaQuest.Common.Type
import Data.List
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.ST
import qualified Data.Map as Map

-- replaces occurrences of TyRef j (j >= i) with TyRef (j + delta)
typeShift :: Int -> Int -> TypeT a -> TypeT a
-- typeShift 0 i t = t
typeShift delta i t = case t of
  TyPrim _ -> t
  TyArr u v -> TyArr (typeShift delta i u) (typeShift delta (i + 1) v)
  TyRef j | j >= i, j + delta >= 0 -> TyRef (j + delta)
          | j >= i, j + delta < 0 -> error "typeShift: negative index"
          | otherwise -> t
  TyExtra _ -> t

typeSubstD :: Int -> TypeT a -> Int -> TypeT a -> TypeT a
typeSubstD depth s i t = case t of
  TyPrim _ -> t
  TyArr u v -> TyArr (typeSubstD depth s i u) (typeSubstD (depth + 1) s (i + 1) v)
  TyRef j | j == i -> typeShift depth 0 s
          | j > i -> TyRef (j - 1)
          | otherwise -> t
  TyExtra _ -> t

-- replaces occurrences of TyRef j (j > i) with TyRef (j-1), and TyRef i with the given type
typeSubst = typeSubstD 0

typeSubstTyScD :: Int -> TypeT a -> Int -> TypeSchemeT a -> TypeSchemeT a
typeSubstTyScD depth s i t = case t of
  Monotype ty -> Monotype (typeSubstD depth s i ty)
  TyScAll sc -> TyScAll (typeSubstTyScD (depth + 1) s (i + 1) sc)

typeSubstTySc = typeSubstTyScD 0

primTypeOf :: PrimValue -> TypeT a
primTypeOf = genPrimTypeOf TyPrim TyArr

data TypeHole = TypeHole deriving (Eq,Show)
type HoledType = TypeT TypeHole
type HTerm = TermT TypeHole
pattern HTyHole = TyExtra TypeHole

data IndexedTypeHole = IndexedTypeHole Int deriving (Eq,Show)
type UType = TypeT IndexedTypeHole
type UTypeScheme = TypeSchemeT IndexedTypeHole
type UTerm = TermT IndexedTypeHole
type UBinding = BindingT IndexedTypeHole
pattern UTyHole i = TyExtra (IndexedTypeHole i)

data Constraint = Constraint UType UType String deriving (Eq,Show)

data TypeInferenceState = TypeInferenceState { tiConstraints :: [Constraint]
                                             , tiNextId :: !Int
                                             , tiAliases :: Map.Map Int UType
                                             }
                          deriving (Eq,Show)

type TypeInference a = ExceptT String (State TypeInferenceState) a

newIndexedTypeHole :: TypeInference IndexedTypeHole
newIndexedTypeHole = do i <- gets tiNextId
                        modify' (\st -> st { tiNextId = i + 1 })
                        return $ IndexedTypeHole i

newTypeHole :: TypeInference UType
newTypeHole = TyExtra <$> newIndexedTypeHole

assignTypeIdTy :: HoledType -> TypeInference UType
assignTypeIdTy ty = case ty of
  TyPrim p -> return (TyPrim p)
  TyArr u v -> TyArr <$> assignTypeIdTy u <*> assignTypeIdTy v
  TyRef j -> return (TyRef j)
  TyExtra TypeHole -> newTypeHole

assignTypeIdTm :: HTerm -> TypeInference UTerm
assignTypeIdTm tm = case tm of
  TPrimValue p -> return (TPrimValue p)
  TAbs name argType body -> TAbs name <$> assignTypeIdTy argType <*> assignTypeIdTm body
  TRef i name -> return (TRef i name)
  TApp f x -> TApp <$> assignTypeIdTm f <*> assignTypeIdTm x
  TLet name def body -> TLet name <$> assignTypeIdTm def <*> assignTypeIdTm body
  TIf cond then_ else_ -> TIf <$> assignTypeIdTm cond <*> assignTypeIdTm then_ <*> assignTypeIdTm else_

assignTypeIdTySc :: UTypeScheme -> TypeInference UType
assignTypeIdTySc (Monotype ty) = return ty
assignTypeIdTySc (TyScAll tysc) = do h <- newTypeHole
                                     assignTypeIdTySc (typeSubstTySc h 0 tysc)

emitConstraint :: UType -> UType -> String -> TypeInference ()
emitConstraint s t err = modify' (\st -> st { tiConstraints = Constraint s t err : tiConstraints st })

constraints :: [UBinding] -> UTerm -> TypeInference UType
constraints ctx tm = case tm of
  TPrimValue primValue -> return (primTypeOf primValue)
  TAbs name argType body -> TyArr argType <$> constraints (VarBind name (Monotype argType) : ctx) body
  TRef i name -> do let tsc = getTypeSchemeFromContext ctx i
                    ty <- assignTypeIdTySc tsc
                    return $ typeShift (i + 1) 0 ty
  TApp f x -> do
    fnType <- constraints ctx f
    actualArgType <- constraints ctx x
    case fnType of
      TyArr expectedArgType retType -> do
        emitConstraint actualArgType expectedArgType ("type error (expected: " ++ show expectedArgType ++ ", got: " ++ show actualArgType ++ ")")
        return $ typeShift (-1) 0 retType
      _ -> do
        h <- newTypeHole
        emitConstraint fnType (TyArr actualArgType h) ("invalid function application (expected function type, got: " ++ show fnType ++ ")")
        return h
  TLet name def body -> do
    definedType <- constraints ctx def
    definedTySc <- generalize ctx definedType
    constraints (VarBind name definedTySc : ctx) body
  TIf cond then_ else_ -> do
    condType <- constraints ctx cond
    thenType <- constraints ctx then_
    elseType <- constraints ctx else_
    emitConstraint condType TyBool "if-then-else: condition must be boolean"
    emitConstraint thenType elseType "if-then-else: type mismatch"
    return thenType

freeVars :: UType -> [Int]
freeVars = foldMap (\(IndexedTypeHole i) -> [i])

freeVarsC :: Constraint -> [Int]
freeVarsC (Constraint t0 t1 e) = freeVars t0 ++ freeVars t1

freeVarsB :: UBinding -> [Int]
freeVarsB = foldMap (\(IndexedTypeHole i) -> [i])

replaceHole :: Int -> UType -> UType -> UType
replaceHole i s t = t >>= (\u@(IndexedTypeHole j) -> if i == j then s else TyExtra u)

replaceHoleC :: Int -> UType -> Constraint -> Constraint
replaceHoleC i s (Constraint t0 t1 e) = Constraint (replaceHole i s t0) (replaceHole i s t1) e

unify :: [Constraint] -> Map.Map Int UType -> Either String (Map.Map Int UType)
unify [] m = return m
unify (Constraint s t e : xs) m
  | s == t = unify xs m
  | UTyHole i <- s, i `notElem` freeVars t = unify (map (replaceHoleC i t) xs) (Map.insert i t $ replaceHole i t <$> m) -- eliminate s
  | UTyHole i <- t, i `notElem` freeVars s = unify (map (replaceHoleC i s) xs) (Map.insert i s $ replaceHole i s <$> m) -- eliminate t
  | TyArr s0 s1 <- s, TyArr t0 t1 <- t = unify (Constraint s0 t0 e : Constraint s1 t1 e : xs) m
  | otherwise = Left e

applyUnificationResultTy :: Map.Map Int UType -> UType -> UType
applyUnificationResultTy m t = t >>= (\u@(IndexedTypeHole i) -> Map.findWithDefault (TyExtra u) i m)

applyUnificationResultC :: Map.Map Int UType -> [Constraint] -> [Constraint]
applyUnificationResultC m [] = []
applyUnificationResultC m (Constraint t0 t1 e : xs)
  | u0 == u1 = applyUnificationResultC m xs
  | otherwise = Constraint u0 u1 e : applyUnificationResultC m xs
  where u0 = applyUnificationResultTy m t0
        u1 = applyUnificationResultTy m t1

applyUnificationResultTm :: Map.Map Int UType -> UTerm -> UTerm
applyUnificationResultTm m = tyMapOnTerm (applyUnificationResultTy m)

generalize :: [UBinding] -> UType -> TypeInference UTypeScheme
generalize ctx ty = do
  ct <- gets tiConstraints
  m <- gets tiAliases
  m' <- case unify ct m of
    Left e -> throwError e
    Right m -> return m
  let ty' = applyUnificationResultTy m' ty
      bb = nub $ concatMap freeVarsB ctx
      constrainedVars = nub $ Map.foldMapWithKey (\i t -> if i `elem` bb then i : freeVars t else []) m'
      generalizedVars = filter (\i -> i `notElem` constrainedVars) $ nub (freeVars ty')
      tyVarCount = length generalizedVars
      go :: Int -> UType -> UType
      go !depth ty = case ty of
        TyPrim p -> ty
        TyArr s t -> TyArr (go depth s) (go (depth + 1) t)
        TyRef i | i >= depth -> TyRef (i + tyVarCount)
                | otherwise -> ty
        TyExtra (IndexedTypeHole i)
          | Just j <- i `elemIndex` generalizedVars -> TyRef (tyVarCount - (j + 1) + depth) -- to be generalized
          | Just u <- Map.lookup i m' -> go depth u -- replace
          | otherwise -> ty -- do not generalize
  modify (\st -> st { tiAliases = m', tiConstraints = applyUnificationResultC m' ct })
  return (putTypeScm tyVarCount (go 0 ty'))
  where
    putTypeScm :: Int -> UType -> UTypeScheme
    putTypeScm 0 t = Monotype t
    putTypeScm k t = TyScAll (putTypeScm (k - 1) t)

instance ExtraTypeParser TypeHole where
  extraSimpleTypeExpr' = reserved "_" >> return HTyHole
  defaultType' = return HTyHole
