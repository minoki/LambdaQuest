-- Translate System Fsub expression into System F
module LambdaQuest.SystemFsub.Coercion where
import qualified LambdaQuest.SystemF as F
import qualified LambdaQuest.SystemF.TypeCheck as F
import qualified LambdaQuest.SystemF.Eval as F
import LambdaQuest.SystemFsub.Type
import qualified LambdaQuest.SystemFsub.Subtype as S
import qualified LambdaQuest.SystemFsub.TypeCheck as TC
import Control.Arrow

lookupTyVarD :: Int -> [Binding] -> Int -> Int
lookupTyVarD d (TyVarBind _ _ : _) 0 = d + 1
lookupTyVarD d (_ : xs) 0 = error "lookupTyVar: not a type variable"
lookupTyVarD d (TyVarBind _ _ : xs) i = lookupTyVarD (d + 2) xs (i - 1)
lookupTyVarD d (_ : xs) i = lookupTyVarD (d + 1) xs (i - 1)
lookupTyVarD d [] _ = error "lookupTyVar: out of range"

lookupTyVar :: [Binding] -> Int -> Int
lookupTyVar = lookupTyVarD 0

lookupVarD :: Int -> [Binding] -> Int -> Int
lookupVarD d (VarBind _ _ : _) 0 = d
lookupVarD d (_ : xs) 0 = error "lookupVar: not a type variable"
lookupVarD d (TyVarBind _ _ : xs) i = lookupVarD (d + 2) xs (i - 1)
lookupVarD d (_ : xs) i = lookupVarD (d + 1) xs (i - 1)
lookupVarD d [] _ = error "lookupVar: out of range"

lookupVar :: [Binding] -> Int -> Int
lookupVar = lookupVarD 0

bindingShift :: Int -> Int -> Binding -> Binding
bindingShift delta i b = case b of
  VarBind name ty -> VarBind name (typeShift delta i ty)
  TyVarBind name bound -> TyVarBind name (typeShift delta i bound)
  AnonymousBind -> b

-- 'forall a <: b. c' is translated to 'forall a. (a -> b) -> c'
mapType :: [Binding] -> Type -> F.Type
mapType ctx (TyPrim p) = F.TyPrim p
mapType ctx (TyArr s t) = F.TyArr (mapType ctx s) (mapType (AnonymousBind : ctx) t)
mapType ctx (TyRef i n) = F.TyRef (lookupTyVar ctx i) n
mapType ctx (TyAll n bound t) = F.TyAll n (F.TyArr (F.TyArr (F.TyRef 0 n) (mapType ctx $ typeShift 1 0 bound)) $ mapType (TyVarBind n bound : ctx) t)
mapType ctx TyTop = F.TyUnit -- `Top' type is mapped to Unit

data CoercionElement = CTerm F.Term !Type
data Coercion = CCoercionSeries [F.Term] !Type
              | CErase !Type -- constant function to Unit

coercionToTerm :: [Binding] -> Coercion -> F.Term
coercionToTerm ctx (CCoercionSeries [t] ty) = t
coercionToTerm ctx (CCoercionSeries fs ty) = F.TAbs "x" (mapType ctx ty) $ foldr (\f x -> F.TApp (F.termShift 1 0 f) x) (F.TRef 0 "x") fs
coercionToTerm ctx (CErase ty) = F.TAbs "x" (mapType ctx ty) (F.TPrimValue PVUnit)

applyCoercion :: Coercion -> F.Term -> F.Term
applyCoercion (CCoercionSeries fs ty) t = foldr F.TApp t fs
applyCoercion (CErase _) t = F.TPrimValue PVUnit

coercionFrom :: Coercion -> Type
coercionFrom (CCoercionSeries _ ty) = ty
coercionFrom (CErase ty) = ty

compositeCoercion :: Coercion -> Coercion -> Coercion
compositeCoercion (CErase _) g = CErase (coercionFrom g)
compositeCoercion _ g@(CErase ty) = g
compositeCoercion (CCoercionSeries fs _) (CCoercionSeries gs ty) = CCoercionSeries (fs ++ gs) ty

coercionShift :: Int -> Int -> Coercion -> Coercion
coercionShift delta i (CCoercionSeries ts ty) = CCoercionSeries (map (F.termShift delta i) ts) (typeShift delta i ty)
coercionShift delta i (CErase ty) = CErase (typeShift delta i ty)

identityFn :: Type -> Coercion
identityFn ty = CCoercionSeries [] ty

coerceIntToReal :: Coercion
coerceIntToReal = CCoercionSeries [F.TPrimValue (PVBuiltinUnary BIntToReal)] TyInt

-- In 'forall a <: B', coerce a to B
coerceTypeParam :: [Binding] -> Int -> Coercion
coerceTypeParam ctx i = CCoercionSeries [F.TRef (lookupTyVar ctx i - 1) "_coercion"] (TyRef i "")

primSubType :: PrimType -> PrimType -> Maybe Coercion
primSubType s t | s == t = Just (identityFn (TyPrim s))
primSubType PTyInt PTyReal = Just coerceIntToReal
primSubType _ _ = Nothing

-- ctx : (index of coercion function, upper bound)
-- the resulting term should have the type (mapType s -> mapType t)
subType :: [Binding] -> Type -> Type -> Maybe Coercion
subType ctx s TyTop = Just (CErase s)
subType ctx (TyPrim s) (TyPrim t) = primSubType s t
subType ctx s@(TyArr s0 s1) (TyArr t0 t1) = do
  a <- subType ctx t0 s0
  b <- subType (AnonymousBind : ctx) s1 t1
  -- \f:s. \x:t0. b (f (a x)) = \f:s. (b . f . a) = b_* . a^*
  case (a, b) of
    (CCoercionSeries [] _, CCoercionSeries [] _) -> return $ identityFn s
    _ -> let c = F.TAbs "f" (mapType ctx s) $ F.TAbs "y" (F.typeShift 1 0 $ mapType ctx t0)
                 $ applyCoercion (coercionShift 2 0 b) $ F.TApp (F.TRef 1 "f") $ applyCoercion (coercionShift 2 0 a) (F.TRef 0 "y")
         in return $ CCoercionSeries [c] s
subType ctx s@(TyRef i _) (TyRef i' _) | i == i' = Just (identityFn s)
subType ctx s@(TyRef i _) t
  | i < length ctx = do
      let bound = getBoundFromContext ctx i
      a <- subType ctx (typeShift (i + 1) 0 bound) t
      return $ a `compositeCoercion` coerceTypeParam ctx i
subType ctx st@(TyAll name b s) (TyAll _ b' t)
  | b == b' = do
      f <- subType (TyVarBind name b : ctx) s t
      -- In System Fsub: (forall a <: b. s) -> (forall a <: b. t)
      -- In System F: (forall a. (a -> b) -> s)) -> (forall a. (a -> b) -> t)
      -- f: s -> t
      -- \x:(forall a. (a -> b) -> s). ?a. \g:(a -> b). f (x [a] g)
      case f of
        CCoercionSeries [] _ -> return $ CCoercionSeries [] st
        _ -> let s' = mapType ctx st
                 c = F.TAbs "x" s' $ F.TTyAbs "a" $ F.TAbs "_coercion" (F.TyArr (F.TyRef 0 "a") (F.typeShift 3 0 $ mapType ctx b))
                     $ applyCoercion (coercionShift 1 2 f) $ F.TApp (F.TTyApp (F.TRef 2 "x") (F.TyRef 1 "a")) (F.TRef 0 "_coercion")
             in return $ CCoercionSeries [c] st
subType ctx _ _ = Nothing

exposeType :: [Binding] -> Type -> Type
exposeType = exposeTypeD 0
  where
    exposeTypeD :: Int -> [Binding] -> Type -> Type
    exposeTypeD d ctx (TyRef i _) = exposeTypeD (i + 1 + d) (drop (i + 1) ctx) (getBoundFromContext ctx i)
    exposeTypeD d ctx t = typeShift d 0 t

primMeetType :: PrimType -> PrimType -> Maybe PrimType
primMeetType s t | s == t = Just s
primMeetType s@PTyInt PTyReal = Just s
primMeetType PTyReal t@PTyInt = Just t
primMeetType _ _ = Nothing

-- (meetType ctx s t) is a type that is maximal among such u that both (u <: s) and (u <: t) are satisfied.
-- Nothing means Bottom (i.e. no such type exist)
meetType :: [Binding] -> Type -> Type -> Maybe Type
meetType ctx s t | S.subType ctx s t = Just s
                 | S.subType ctx t s = Just t
meetType ctx (TyPrim s) (TyPrim t) = TyPrim <$> primMeetType s t
meetType ctx (TyArr s0 s1) (TyArr t0 t1) = TyArr (joinType ctx s0 t0) <$> meetType (AnonymousBind : ctx) s1 t1
meetType ctx (TyAll n b s) (TyAll _ b' t) | b == b' = TyAll n b <$> meetType (TyVarBind n b : ctx) s t
meetType _ _ _ = Nothing

primJoinType :: PrimType -> PrimType -> Type
primJoinType s t | s == t = TyPrim s
primJoinType PTyInt t@PTyReal = TyPrim t
primJoinType s@PTyReal PTyInt = TyPrim s
primJoinType _ _ = TyTop

-- (joinType ctx s t) is a type that is minimal among such u that both (s <: u) and (t <: u) are satisfied.
joinType :: [Binding] -> Type -> Type -> Type
joinType ctx s t | S.subType ctx s t = t
                 | S.subType ctx t s = s
joinType ctx (TyPrim s) (TyPrim t) = primJoinType s t
joinType ctx (TyArr s0 s1) (TyArr t0 t1)
  = case meetType ctx s0 t0 of
      Just u -> TyArr u $ joinType (AnonymousBind : ctx) s1 t1
      Nothing -> TyTop
joinType ctx (TyRef i _) t = joinType ctx (typeShift (i + 1) 0 (getBoundFromContext ctx i)) t
joinType ctx s (TyRef i _) = joinType ctx s (typeShift (i + 1) 0 (getBoundFromContext ctx i))
joinType ctx (TyAll n b s) (TyAll _ b' t)
  | b == b' = TyAll n b $ joinType (TyVarBind n b : ctx) s t
joinType _ _ _ = TyTop

mapTerm :: [Binding] -> Term -> Either String (F.Term, Type)
mapTerm ctx (TPrimValue v) = return $ (F.TPrimValue v, TC.primTypeOf v)
mapTerm ctx (TAbs name argType body) = do
  let argType' = mapType ctx argType
  (body', retType) <- mapTerm (VarBind name argType : ctx) body
  return (F.TAbs name argType' body', TyArr argType retType)
mapTerm ctx (TTyAbs name bound body) = do
  let bound' = mapType ctx bound
  (body', bodyType) <- mapTerm (TyVarBind name bound : ctx) body
  -- ?a <: b. body
  -- ?a. \coercion: a -> b. body
  let c = F.TTyAbs name $ F.TAbs "_coercion" (F.TyArr (F.TyRef 0 name) (F.typeShift 2 0 bound')) $ body'
  return (c, TyAll name bound bodyType)
mapTerm ctx (TRef i name) = return (F.TRef (lookupVar ctx i) name, typeShift (i + 1) 0 $ getTypeFromContext ctx i)
mapTerm ctx (TApp f x) = do
  (f', fnType) <- mapTerm ctx f
  (x', actualArgType) <- mapTerm ctx x
  let e = exposeType ctx fnType
      Just ec = subType ctx fnType e
  -- f x ~~> (ec f) (ac x)
  case e of
    TyArr expectedArgType retType
      | Just ac <- subType ctx actualArgType expectedArgType -> return (F.TApp (applyCoercion ec f') (applyCoercion ac x'), typeShift (-1) 0 retType)
      | otherwise -> Left $ "type error (expected: " ++ show expectedArgType ++ ", got: " ++ show actualArgType ++ ")"
    _ -> Left $ "invalid function application (expected function type, got: " ++ show fnType ++ ")"
mapTerm ctx (TTyApp s ty) = do
  (s', fnType) <- mapTerm ctx s
  let ty' = mapType ctx ty
  let e = exposeType ctx fnType
      Just ec = subType ctx fnType e
  -- s [ty] ~~> (ec s) [ty] c
  case e of
    TyAll _name bound bodyType
      | Just c <- subType ctx ty bound -> return (F.TApp (F.TTyApp (applyCoercion ec s') ty') (coercionToTerm ctx c), typeSubst ty 0 bodyType)
      | otherwise -> Left $ "invalid type application (" ++ show ty ++ " is not a subtype of " ++ show bound ++ ")"
    _ -> Left $ "invalid type application (expected forall type, got: " ++ show fnType ++ ")"
mapTerm ctx (TIf cond then_ else_) = do
  (cond', condType) <- mapTerm ctx cond
  (then', thenType) <- mapTerm ctx then_
  (else', elseType) <- mapTerm ctx else_
  let e = exposeType ctx condType
      Just ec = subType ctx condType e
  case e of
    TyBool -> do
      let j = joinType ctx thenType elseType
          Just thenJ = subType ctx thenType j
          Just elseJ = subType ctx elseType j
      return (F.TIf (applyCoercion ec cond') (applyCoercion thenJ then') (applyCoercion elseJ else'), j)
    _ -> Left "if-then-else: condition must be boolean"
mapTerm ctx (TCoerce s ty) = do
  (s', xt) <- mapTerm ctx s
  case subType ctx xt ty of
    Just c -> return (applyCoercion c s', ty)
    _ -> Left $ "type coercion: the actual type " ++ show xt ++ " is not a subtype of " ++ show ty
-- Property: snd <$> mapTerm ctx t == typeOf ctx t
