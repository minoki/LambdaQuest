-- Subtyping relation
{-# LANGUAGE BangPatterns #-}
module LambdaQuest.Finter.Subtype where
import LambdaQuest.Finter.Type
import LambdaQuest.SystemFsub.Subtype (primSubType,primMeetType,primJoinType)
import Data.Foldable (foldl')

normalizeType :: [Binding] -> Type -> CCanonicalType
normalizeType ctx (TyPrim p) = [CTyPrim p]
normalizeType ctx (TyArr s t) = do
  let !s' = normalizeType ctx s
  t' <- normalizeType (AnonymousBind : ctx) t
  return (CTyArr s' t')
normalizeType ctx (TyRef i name) = [CTyRef i name]
normalizeType ctx (TyAll name bound t) = do
  let !bound' = normalizeType ctx bound
  t' <- normalizeType (TyVarBind name bound' : ctx) t
  return (CTyAll name bound' t')
normalizeType ctx (TyInter ts) = foldl' (meetTypeC ctx) [] $ map (normalizeType ctx) ts
normalizeType ctx TyTop = []

subTypeC :: [Binding] -> CCanonicalType -> CCanonicalType -> Bool
subTypeC ctx xs ys = all (\y -> any (\x -> subTypeI ctx x y) xs) ys

subTypeI :: [Binding] -> ICanonicalType -> ICanonicalType -> Bool
subTypeI ctx (CTyPrim s) (CTyPrim t) = primSubType s t
subTypeI ctx (CTyArr s0 s1) (CTyArr t0 t1) = subTypeC ctx t0 s0 && subTypeI (AnonymousBind : ctx) s1 t1
subTypeI ctx (CTyRef i _) (CTyRef i' _) | i == i' = True
subTypeI ctx (CTyRef i _) t = let bound = typeShiftC (i + 1) 0 $ getBoundFromCContext ctx i
                              in subTypeC ctx bound [t]
subTypeI ctx (CTyAll name b s) (CTyAll _ b' t) | b == b' = subTypeI (TyVarBind name b : ctx) s t
subTypeI ctx _ _ = False

meetTypeC :: [Binding] -> CCanonicalType -> CCanonicalType -> CCanonicalType
meetTypeC ctx = rr
  where
    r :: ICanonicalType -> [ICanonicalType] -> (Bool,[ICanonicalType])
    r x [] = (True,[])
    r x (y:ys) | subTypeI ctx x y = r x ys -- delete y
               | subTypeI ctx y x = (False,y:ys) -- delete x
               | otherwise = (y :) <$> r x ys
    rr :: [ICanonicalType] -> [ICanonicalType] -> [ICanonicalType]
    rr [] ys = ys
    rr (x:xs) ys | x' = x : rr xs ys'
                 | otherwise = rr xs ys'
      where (x',ys') = r x ys

joinTypeC :: [Binding] -> CCanonicalType -> CCanonicalType -> CCanonicalType
joinTypeC ctx s t | subTypeC ctx s t = t
                  | subTypeC ctx t s = s
                  | otherwise = do s' <- s
                                   t' <- t
                                   joinTypeI ctx s' t'

joinTypeI :: [Binding] -> ICanonicalType -> ICanonicalType -> CCanonicalType
joinTypeI ctx s t | subTypeI ctx s t = [t]
                  | subTypeI ctx t s = [s]
joinTypeI ctx (CTyPrim s) (CTyPrim t) = case primJoinType s t of
                                          Just u -> [CTyPrim u]
                                          Nothing -> []
joinTypeI ctx (CTyArr s0 s1) (CTyArr t0 t1) = CTyArr (meetTypeC ctx s0 t0) <$> (joinTypeI (AnonymousBind : ctx) s1 t1)
joinTypeI ctx (CTyRef i _) t = joinTypeC ctx (typeShiftC (i + 1) 0 (getBoundFromCContext ctx i)) [t]
joinTypeI ctx s (CTyRef i _) = joinTypeC ctx [s] (typeShiftC (i + 1) 0 (getBoundFromCContext ctx i))
joinTypeI ctx (CTyAll n b s) (CTyAll _ b' t)
  | b == b' = CTyAll n b <$> joinTypeI (TyVarBind n b : ctx) s t
joinTypeI _ _ _ = []

subType :: [Binding] -> Type -> Type -> Bool
subType ctx s t = subTypeC ctx (normalizeType ctx s) (normalizeType ctx t)

-- (meetType ctx s t) is a type that is maximal among such u that both (u <: s) and (u <: t) are satisfied.
meetType :: [Binding] -> Type -> Type -> Type
meetType ctx s t = canonicalToOrdinary $ meetTypeC ctx (normalizeType ctx s) (normalizeType ctx t)

-- (joinType ctx s t) is a type that is minimal among such u that both (s <: u) and (t <: u) are satisfied.
joinType :: [Binding] -> Type -> Type -> Type
joinType ctx s t = canonicalToOrdinary $ joinTypeC ctx (normalizeType ctx s) (normalizeType ctx t)

exposeType :: [Binding] -> Type -> [Type]
exposeType = exposeTypeD 0
  where
    exposeTypeD :: Int -> [Binding] -> Type -> [Type]
    exposeTypeD d ctx (TyRef i _) = exposeTypeD (i + 1 + d) (drop (i + 1) ctx) (getBoundFromContext ctx i)
    exposeTypeD d ctx (TyInter tys) = concatMap (exposeTypeD d ctx) tys
    exposeTypeD d ctx t = [typeShift d 0 t]
