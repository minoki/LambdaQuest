module LambdaQuest.SystemF.TypeCheck where
import LambdaQuest.SystemF.Type
import LambdaQuest.Common.Type

-- replaces occurrences of TyRef j (j >= i) with TyRef (j + delta)
typeShift :: Int -> Int -> Type -> Type
typeShift delta i t = case t of
  TyPrim _ -> t
  TyArr u v -> TyArr (typeShift delta i u) (typeShift delta i v)
  TyRef j name | j >= i -> TyRef (j + delta) name
               | otherwise -> t
  TyAll n t -> TyAll n (typeShift delta (i + 1) t)
-- typeShift 0 i t == t

typeSubstD :: Int -> Type -> Int -> Type -> Type
typeSubstD depth s i t = case t of
  TyPrim _ -> t
  TyArr u v -> TyArr (typeSubstD depth s i u) (typeSubstD depth s i v)
  TyRef j name | j == i -> typeShift depth 0 s
               | j > i -> TyRef (j - 1) name
               | otherwise -> t
  TyAll n t -> TyAll n (typeSubstD (depth + 1) s (i + 1) t)

-- replaces occurrences of TyRef j (j > i) with TyRef (j-1), and TyRef i with the given type
typeSubst = typeSubstD 0

primTypeOf :: PrimValue -> Type
primTypeOf = genPrimTypeOf TyPrim TyArr

typeOf :: [Type] -> Term -> Either String Type
typeOf ctx tm = case tm of
  TPrimValue primValue -> return (primTypeOf primValue)
  TAbs name argType body -> do
    retType <- typeOf (argType : ctx) body
    return (TyArr argType retType)
  TTyAbs name body -> do
    let ctx' = map (typeShift 1 0) ctx
    retType <- typeOf ctx' body
    return (TyAll name retType)
  TRef i name | i < length ctx -> return (ctx !! i)
              | otherwise -> Left $ "TRef out of range"
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
