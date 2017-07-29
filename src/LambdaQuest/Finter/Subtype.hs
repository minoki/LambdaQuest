-- Subtyping relation
{-# LANGUAGE BangPatterns #-}
module LambdaQuest.Finter.Subtype where
import LambdaQuest.Finter.Type
import LambdaQuest.SystemFsub.Subtype (primSubType,primMeetType,primJoinType)
import Data.Foldable (foldl')

-- compound canonical type
type CCanonicalType = [ICanonicalType]

-- individual canonical type
data ICanonicalType = CTyPrim !PrimType
                    | CTyArr CCanonicalType ICanonicalType
                    | CTyRef !Int String
                    | CTyAll String CCanonicalType ICanonicalType
                    deriving (Show)

data CBinding = CVarBind String CCanonicalType
              | CTyVarBind String CCanonicalType
              | CTyFor String CCanonicalType
              | CAnonymousBind
              deriving (Show)

normalizeType :: [CBinding] -> Type -> CCanonicalType
normalizeType ctx (TyPrim p) = [CTyPrim p]
normalizeType ctx (TyArr s t) = do
  let !s' = normalizeType ctx s
  t' <- normalizeType (CAnonymousBind : ctx) t
  return (CTyArr s' t')
normalizeType ctx (TyRef i name) = [CTyRef i name]
normalizeType ctx (TyAll name bound t) = do
  let !bound' = normalizeType ctx bound
  t' <- normalizeType (CTyVarBind name bound' : ctx) t
  return (CTyAll name bound' t')
normalizeType ctx (TyInter ts) = foldl' (mergeCCanonicalTypes ctx) [] $ map (normalizeType ctx) ts
normalizeType ctx TyTop = []

mergeCCanonicalTypes :: [CBinding] -> CCanonicalType -> CCanonicalType -> CCanonicalType
mergeCCanonicalTypes ctx = rr
  where
    r :: ICanonicalType -> [ICanonicalType] -> (Bool,[ICanonicalType])
    r x [] = (True,[])
    r x (y:ys) | subTypeI ctx x y = r x ys -- delete y
               | subTypeI ctx y x = (False,ys) -- delete x
               | otherwise = (y :) <$> r x ys
    rr :: [ICanonicalType] -> [ICanonicalType] -> [ICanonicalType]
    rr [] ys = ys
    rr (x:xs) ys = let (x',ys') = r x ys in (if x' then (x:) else id) (rr xs ys')

cCanonicalToOrdinary :: CCanonicalType -> Type
cCanonicalToOrdinary [] = TyTop -- top
cCanonicalToOrdinary [t] = iCanonicalToOrdinary t
cCanonicalToOrdinary tys = TyInter $ map iCanonicalToOrdinary tys

iCanonicalToOrdinary :: ICanonicalType -> Type
iCanonicalToOrdinary (CTyPrim p) = TyPrim p
iCanonicalToOrdinary (CTyArr s t) = TyArr (cCanonicalToOrdinary s) (iCanonicalToOrdinary t)
iCanonicalToOrdinary (CTyRef i name) = TyRef i name
iCanonicalToOrdinary (CTyAll name b t) = TyAll name (cCanonicalToOrdinary b) (iCanonicalToOrdinary t)

normalizeContext :: [Binding] -> [CBinding]
normalizeContext [] = []
normalizeContext (VarBind name ty : ctx) = CVarBind name (normalizeType r ty) : r where r = normalizeContext ctx
normalizeContext (TyVarBind name bound : ctx) = CTyVarBind name (normalizeType r bound) : r where r = normalizeContext ctx
normalizeContext (TyFor name ty : ctx) = CTyFor name (normalizeType r ty) : r where r = normalizeContext ctx
normalizeContext (AnonymousBind : ctx) = CAnonymousBind : normalizeContext ctx

cTypeShiftC :: Int -> Int -> CCanonicalType -> CCanonicalType
cTypeShiftC delta i = map (cTypeShiftI delta i)

cTypeShiftI :: Int -> Int -> ICanonicalType -> ICanonicalType
cTypeShiftI delta i t = case t of
  CTyPrim _ -> t
  CTyArr s t -> CTyArr (cTypeShiftC delta i s) (cTypeShiftI delta i t)
  CTyRef j name | j >= i, j + delta >= 0 -> CTyRef (j + delta) name
                | j >= i, j + delta < 0 -> error "typeShift: negative index"
                | otherwise -> t
  CTyAll name b t -> CTyAll name (cTypeShiftC delta i b) (cTypeShiftI delta (i + 1) t)

getBoundFromCContext :: [CBinding] -> Int -> CCanonicalType
getBoundFromCContext ctx i
  | i < length ctx = case ctx !! i of
                       CTyVarBind _ b -> b
                       b -> error ("TyRef: expected a type variable binding, found " ++ show b)
  | otherwise = error "TyRef: index out of bounds"

subTypeC :: [CBinding] -> CCanonicalType -> CCanonicalType -> Bool
subTypeC ctx xs ys = all (\y -> any (\x -> subTypeI ctx x y) xs) ys

subTypeI :: [CBinding] -> ICanonicalType -> ICanonicalType -> Bool
subTypeI ctx (CTyPrim s) (CTyPrim t) = primSubType s t
subTypeI ctx (CTyArr s0 s1) (CTyArr t0 t1) = subTypeC ctx t0 s0 && subTypeI (CAnonymousBind : ctx) s1 t1
subTypeI ctx (CTyRef i _) (CTyRef i' _) | i == i' = True
subTypeI ctx (CTyRef i _) t = let bound = cTypeShiftC (i + 1) 0 $ getBoundFromCContext ctx i
                              in subTypeC ctx bound [t]
subTypeI ctx (CTyAll name b s) (CTyAll _ b' t) | b == b' = subTypeI (CTyVarBind name b : ctx) s t
subTypeI ctx _ _ = False

subType :: [Binding] -> Type -> Type -> Bool
subType ctx s t = subTypeC ctx' (normalizeType ctx' s) (normalizeType ctx' t)
  where ctx' = normalizeContext ctx

meetTypeC :: [CBinding] -> CCanonicalType -> CCanonicalType -> CCanonicalType
meetTypeC ctx s t = mergeCCanonicalTypes ctx s t

joinTypeC :: [CBinding] -> CCanonicalType -> CCanonicalType -> CCanonicalType
joinTypeC ctx s t | subTypeC ctx s t = t
                  | subTypeC ctx t s = s
                  | otherwise = do s' <- s
                                   t' <- t
                                   joinTypeI ctx s' t'

joinTypeI :: [CBinding] -> ICanonicalType -> ICanonicalType -> CCanonicalType
joinTypeI ctx s t | subTypeI ctx s t = [t]
                  | subTypeI ctx t s = [s]
joinTypeI ctx (CTyPrim s) (CTyPrim t) = case primJoinType s t of
                                          Just u -> [CTyPrim u]
                                          Nothing -> []
joinTypeI ctx (CTyArr s0 s1) (CTyArr t0 t1) = CTyArr (meetTypeC ctx s0 t0) <$> (joinTypeI (CAnonymousBind : ctx) s1 t1)
joinTypeI ctx (CTyRef i _) t = joinTypeC ctx (cTypeShiftC (i + 1) 0 (getBoundFromCContext ctx i)) [t]
joinTypeI ctx s (CTyRef i _) = joinTypeC ctx [s] (cTypeShiftC (i + 1) 0 (getBoundFromCContext ctx i))
joinTypeI ctx (CTyAll n b s) (CTyAll _ b' t)
  | b == b' = CTyAll n b <$> joinTypeI (CTyVarBind n b : ctx) s t
joinTypeI _ _ _ = []

-- (meetType ctx s t) is a type that is maximal among such u that both (u <: s) and (u <: t) are satisfied.
meetType :: [Binding] -> Type -> Type -> Type
meetType ctx s t = cCanonicalToOrdinary $ meetTypeC ctx' (normalizeType ctx' s) (normalizeType ctx' t)
  where ctx' = normalizeContext ctx

-- (joinType ctx s t) is a type that is minimal among such u that both (s <: u) and (t <: u) are satisfied.
joinType :: [Binding] -> Type -> Type -> Type
joinType ctx s t = cCanonicalToOrdinary $ joinTypeC ctx' (normalizeType ctx' s) (normalizeType ctx' t)
  where ctx' = normalizeContext ctx

exposeType :: [Binding] -> Type -> [Type]
exposeType = exposeTypeD 0
  where
    exposeTypeD :: Int -> [Binding] -> Type -> [Type]
    exposeTypeD d ctx (TyRef i _) = exposeTypeD (i + 1 + d) (drop (i + 1) ctx) (getBoundFromContext ctx i)
    exposeTypeD d ctx (TyInter tys) = concatMap (exposeTypeD d ctx) tys
    exposeTypeD d ctx t = [typeShift d 0 t]


instance Eq ICanonicalType where
  CTyPrim p    == CTyPrim p'     = p == p'
  CTyArr s t   == CTyArr s' t'   = s == s' && t == t'
  CTyRef i _   == CTyRef i' _    = i == i'
  CTyAll _ b t == CTyAll _ b' t' = b == b' && t == t'
  _            == _              = False
