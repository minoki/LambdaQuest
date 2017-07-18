-- Subtyping relation
module LambdaQuest.SystemFsub.Subtype where
import LambdaQuest.SystemFsub.Type

primSubType :: PrimType -> PrimType -> Bool
primSubType s t | s == t = True
primSubType PTyInt PTyReal = True
primSubType _ _ = False

subType :: [Type] -> Type -> Type -> Bool
subType ctx s TyTop = True
subType ctx (TyPrim s) (TyPrim t) = primSubType s t
subType ctx (TyArr s0 s1) (TyArr t0 t1) = subType ctx t0 s0 && subType ctx s1 t1
subType ctx (TyRef i _) (TyRef i' _) | i == i' = True
subType ctx (TyRef i _) t | i < length ctx = let bound = ctx !! i
                                             in subType ctx bound t
subType ctx (TyAll _ b s) (TyAll _ b' t) | b == b' = subType (b : ctx) s t
subType ctx _ _ = False

data TypeOrdering = TOEquiv
                  | TOSub
                  | TOSuper
                  | TOUnrelated
                  deriving (Eq,Show,Enum,Bounded)

compareType :: [Type] -> Type -> Type -> TypeOrdering
compareType ctx s t | subType ctx s t && subType ctx t s = TOEquiv
                    | subType ctx s t = TOSub
                    | subType ctx t s = TOSuper
                    | otherwise = TOUnrelated

exposeType :: [Type] -> Type -> Type
exposeType = exposeTypeD 0
  where
    exposeTypeD :: Int -> [Type] -> Type -> Type
    exposeTypeD d ctx (TyRef i _) = exposeTypeD (i + d) (drop i ctx) (ctx !! i)
    exposeTypeD d ctx t = typeShift d 0 t

primMeetType :: PrimType -> PrimType -> Maybe PrimType
primMeetType s t | s == t = Just s
primMeetType s@PTyInt PTyReal = Just s
primMeetType PTyReal t@PTyInt = Just t
primMeetType _ _ = Nothing

-- (meetType ctx s t) is a type that is maximal among such u that both (u <: s) and (u <: t) are satisfied.
-- Nothing means Bottom (i.e. no such type exist)
meetType :: [Type] -> Type -> Type -> Maybe Type
meetType ctx s t | subType ctx s t = Just s
                 | subType ctx t s = Just t
meetType ctx (TyPrim s) (TyPrim t) = TyPrim <$> primMeetType s t
meetType ctx (TyArr s0 s1) (TyArr t0 t1) = TyArr (joinType ctx s0 t0) <$> meetType ctx s1 t1
meetType ctx (TyAll n b s) (TyAll _ b' t) | b == b' = TyAll n b <$> meetType (b : ctx) s t
meetType _ _ _ = Nothing

primJoinType :: PrimType -> PrimType -> Type
primJoinType s t | s == t = TyPrim s
primJoinType PTyInt t@PTyReal = TyPrim t
primJoinType s@PTyReal PTyInt = TyPrim s
primJoinType _ _ = TyTop

-- (joinType ctx s t) is a type that is minimal among such u that both (s <: u) and (t <: u) are satisfied.
joinType :: [Type] -> Type -> Type -> Type
joinType ctx s t | subType ctx s t = t
                 | subType ctx t s = s
joinType ctx (TyPrim s) (TyPrim t) = primJoinType s t
joinType ctx (TyArr s0 s1) (TyArr t0 t1)
  = case meetType ctx s0 t0 of
      Just u -> TyArr u $ joinType ctx s1 t1
      Nothing -> TyTop
joinType ctx (TyRef i _) t = joinType ctx (typeShift i 0 (ctx !! i)) t
joinType ctx s (TyRef i _) = joinType ctx s (typeShift i 0 (ctx !! i))
joinType ctx (TyAll n b s) (TyAll _ b' t)
  | b == b' = TyAll n b $ joinType (b : ctx) s t
joinType _ _ _ = TyTop
