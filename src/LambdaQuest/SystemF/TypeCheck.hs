module LambdaQuest.SystemF.TypeCheck where
import LambdaQuest.SystemF.Type

-- replaces occurrences of TyRef j (j >= i) with TyRef (j + delta)
typeShift :: Int -> Int -> Type -> Type
typeShift delta i t = case t of
  TyPrim _ -> t
  TyArr u v -> TyArr (typeShift delta i u) (typeShift delta i v)
  TyRef j | j >= i -> TyRef (j + delta)
          | otherwise -> t
  TyAll n t -> TyAll n (typeShift delta (i + 1) t)
-- typeShift 0 i t == t

typeSubstD :: Int -> Type -> Int -> Type -> Type
typeSubstD depth s i t = case t of
  TyPrim _ -> t
  TyArr u v -> TyArr (typeSubstD depth s i u) (typeSubstD depth s i v)
  TyRef j | j == i -> typeShift depth 0 s
          | j > i -> TyRef (j - 1)
          | otherwise -> t
  TyAll n t -> TyAll n (typeSubstD (depth + 1) s (i + 1) t)

-- replaces occurrences of TyRef j (j > i) with TyRef (j-1), and TyRef i with the given type
typeSubst = typeSubstD 0

builtinUnaryFnType :: BuiltinUnaryFn -> Type
builtinUnaryFnType t = case t of
  BNegateInt -> TyArr TyInt TyInt
  BNegateReal -> TyArr TyReal TyReal
  BIntToReal -> TyArr TyInt TyReal

builtinBinaryFnType :: BuiltinBinaryFn -> Type
builtinBinaryFnType t = case t of
  BAddInt -> TyArr TyInt (TyArr TyInt TyInt)
  BSubInt -> TyArr TyInt (TyArr TyInt TyInt)
  BMulInt -> TyArr TyInt (TyArr TyInt TyInt)
  BLtInt -> TyArr TyInt (TyArr TyInt TyBool)
  BLeInt -> TyArr TyInt (TyArr TyInt TyBool)
  BEqualInt -> TyArr TyInt (TyArr TyInt TyBool)
  BAddReal -> TyArr TyReal (TyArr TyReal TyReal)
  BSubReal -> TyArr TyReal (TyArr TyReal TyReal)
  BMulReal -> TyArr TyReal (TyArr TyReal TyReal)
  BDivReal -> TyArr TyReal (TyArr TyReal TyReal)
  BLtReal -> TyArr TyReal (TyArr TyReal TyBool)
  BLeReal -> TyArr TyReal (TyArr TyReal TyBool)
  BEqualReal -> TyArr TyReal (TyArr TyReal TyBool)

typeOf :: [Type] -> Term -> Either String Type
typeOf ctx tm = case tm of
  TPrimValue primValue -> case primValue of
    PVInt _ -> return TyInt
    PVReal _ -> return TyReal
    PVBool _ -> return TyBool
    PVBuiltinUnary f -> return $ builtinUnaryFnType f
    PVBuiltinBinary f -> return $ builtinBinaryFnType f
  TAbs name argType body -> do
    retType <- typeOf (argType : ctx) body
    return (TyArr argType retType)
  TTyAbs name body -> do
    retType <- typeOf ctx body
    return (TyAll name retType)
  TRef i | i < length ctx -> return (ctx !! i)
         | otherwise -> Left "TRef out of range"
  TApp f x -> do
    fnType <- typeOf ctx f
    actualArgType <- typeOf ctx x
    case fnType of
      TyArr expectedArgType retType | actualArgType == expectedArgType -> return retType
                                    | otherwise -> Left ("type error (expected: " ++ show expectedArgType ++ ", got: " ++ show actualArgType ++ ")")
      _ -> Left ("invalid function application (expected function type, got: " ++ show fnType ++ ")")
  TTyApp f t -> do
    fnType <- typeOf ctx f
    case fnType of
      TyAll _name bodyType -> return (typeSubst t 0 bodyType)
      _ -> Left ("invalid type application (expected forall type, got: " ++ show fnType ++ ")")
  TIf cond then_ else_ -> do
    condType <- typeOf ctx cond
    thenType <- typeOf ctx then_
    elseType <- typeOf ctx else_
    case condType of
      TyBool | thenType == elseType -> return thenType
             | otherwise -> Left "if-then-else: type mismatch"
      _ -> Left "if-then-else: conditon must be boolean"
