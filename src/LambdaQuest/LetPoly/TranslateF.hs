-- Translation to System F
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module LambdaQuest.LetPoly.TranslateF where
import LambdaQuest.LetPoly.Type
import LambdaQuest.LetPoly.Parse
import LambdaQuest.LetPoly.TypeCheck hiding (constraints,generalizeTm)
import LambdaQuest.Common.Type
import qualified LambdaQuest.SystemF.Type as F
import Data.List
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.ST
import qualified Data.Map as Map

translateMonotype :: TypeT a -> F.TypeT a
translateMonotype ty = case ty of
  TyPrim p -> F.TyPrim p
  TyArr s t -> F.TyArr (translateMonotype s) (translateMonotype t)
  TyRef i -> F.TyRef i ""
  TyExtra x -> F.TyExtra x

constraints :: [UBinding] -> UTerm -> TypeInference (F.TermT IndexedTypeHole,UType)
constraints ctx tm = case tm of
  TPrimValue primValue -> return (F.TPrimValue primValue,primTypeOf primValue)
  TAbs name argType body -> do
    (body',bodyType) <- constraints (VarBind name (Monotype argType) : ctx) body
    return (F.TAbs name (translateMonotype argType) body', TyArr argType bodyType)
  TRef i name -> do let tsc = getTypeSchemeFromContext ctx i
                        assignTypeId tm (Monotype ty) = return (tm,ty)
                        assignTypeId tm (TyScAll tysc) = do
                          h <- newTypeHole
                          assignTypeId (F.TTyApp tm (translateMonotype h)) (typeSubstTySc h 0 tysc)
                    (tm,ty) <- assignTypeId (F.TRef i name) tsc
                    return (tm,typeShift (i + 1) 0 ty)
  TApp f x -> do
    (f',fnType) <- constraints ctx f
    (x',actualArgType) <- constraints ctx x
    case fnType of
      TyArr expectedArgType retType -> do
        emitConstraint actualArgType expectedArgType ("type error (expected: " ++ show expectedArgType ++ ", got: " ++ show actualArgType ++ ")")
        return (F.TApp f' x',typeShift (-1) 0 retType)
      _ -> do
        h <- newTypeHole
        emitConstraint fnType (TyArr actualArgType h) ("invalid function application (expected function type, got: " ++ show fnType ++ ")")
        return (F.TApp f' x',h)
  TLet name def body -> do
    (def',definedType) <- constraints ctx def
    (def'',definedTySc) <- generalizeTm ctx def' definedType
    (body',bodyType) <- constraints (VarBind name definedTySc : ctx) body
    return (F.TLet name def'' body', bodyType)
  TIf cond then_ else_ -> do
    (cond',condType) <- constraints ctx cond
    (then',thenType) <- constraints ctx then_
    (else',elseType) <- constraints ctx else_
    emitConstraint condType TyBool "if-then-else: condition must be boolean"
    emitConstraint thenType elseType "if-then-else: type mismatch"
    return (F.TIf cond' then' else',thenType)

generalizeTm :: [UBinding] -> F.TermT IndexedTypeHole -> UType -> TypeInference (F.TermT IndexedTypeHole,UTypeScheme)
generalizeTm ctx tm ty = do
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
      goFTy :: Int -> F.TypeT IndexedTypeHole -> F.TypeT IndexedTypeHole
      goFTy !depth ty = case ty of
        F.TyPrim _ -> ty
        F.TyArr s t -> F.TyArr (goFTy depth s) (goFTy (depth + 1) t)
        F.TyAll name t -> F.TyAll name (goFTy (depth + 1) t)
        F.TyRef i name | i >= depth -> F.TyRef (i + tyVarCount) name
                       | otherwise -> ty
        F.TyExtra (IndexedTypeHole i)
          | Just j <- i `elemIndex` generalizedVars -> F.TyRef (tyVarCount - (j + 1) + depth) "" -- to be generalized
          | Just u <- Map.lookup i m' -> translateMonotype (go depth u) -- replace
          | otherwise -> ty -- do not generalize
      goFTm :: Int -> F.TermT IndexedTypeHole -> F.TermT IndexedTypeHole
      goFTm !depth tm = case tm of
        F.TPrimValue _ -> tm
        F.TAbs name ty body -> F.TAbs name (goFTy depth ty) (goFTm (depth + 1) body)
        F.TTyAbs name body -> F.TTyAbs name (goFTm (depth + 1) body)
        F.TRef _ _ -> tm
        F.TApp f x -> F.TApp (goFTm depth f) (goFTm depth x)
        F.TTyApp x ty -> F.TTyApp (goFTm depth x) (goFTy depth ty)
        F.TLet name def body -> F.TLet name (goFTm depth def) (goFTm (depth + 1) body)
        F.TIf cond then_ else_ -> F.TIf (goFTm depth cond) (goFTm depth then_) (goFTm depth else_)
  modify (\st -> st { tiAliases = m', tiConstraints = applyUnificationResultC m' ct })
  return (putTypeAbs tyVarCount (goFTm 0 tm),putTypeScm tyVarCount (go 0 ty'))
  where
    putTypeScm :: Int -> UType -> UTypeScheme
    putTypeScm 0 t = Monotype t
    putTypeScm k t = TyScAll (putTypeScm (k - 1) t)
    putTypeAbs :: Int -> F.TermT IndexedTypeHole -> F.TermT IndexedTypeHole
    putTypeAbs 0 t = t
    putTypeAbs k t = F.TTyAbs "a" (putTypeAbs (k - 1) t)
