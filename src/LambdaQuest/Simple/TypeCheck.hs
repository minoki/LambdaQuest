module LambdaQuest.Simple.TypeCheck where
import LambdaQuest.Simple.Type
import LambdaQuest.Common.Type

-- replaces occurrences of TyRef j (j >= i) with TyRef (j + delta)
typeShift :: Int -> Int -> Type -> Type
typeShift delta i t = t

typeSubstD :: Int -> Type -> Int -> Type -> Type
typeSubstD depth s i t = t

-- replaces occurrences of TyRef j (j > i) with TyRef (j-1), and TyRef i with the given type
typeSubst = typeSubstD 0

primTypeOf :: PrimValue -> Type
primTypeOf = genPrimTypeOf TyPrim TyArr

typeOf :: [Binding] -> Term -> Either String Type
typeOf ctx tm = case tm of
  TPrimValue primValue -> return (primTypeOf primValue)
  TAbs name argType body -> TyArr argType <$> typeOf (VarBind name argType : ctx) body
  TRef i name -> return $ typeShift (i + 1) 0 $ getTypeFromContext ctx i
  TApp f x -> do
    fnType <- typeOf ctx f
    actualArgType <- typeOf ctx x
    case fnType of
      TyArr expectedArgType retType | actualArgType == expectedArgType -> return $ typeShift (-1) 0 retType
                                    | otherwise -> Left ("type error (expected: " ++ show expectedArgType ++ ", got: " ++ show actualArgType ++ ")")
      _ -> Left ("invalid function application (expected function type, got: " ++ show fnType ++ ")")
  TLet name def body -> do
    definedType <- typeOf ctx def
    typeOf (VarBind name definedType : ctx) body
  TIf cond then_ else_ -> do
    condType <- typeOf ctx cond
    thenType <- typeOf ctx then_
    elseType <- typeOf ctx else_
    case condType of
      TyBool | thenType == elseType -> return thenType
             | otherwise -> Left "if-then-else: type mismatch"
      _ -> Left "if-then-else: conditon must be boolean"
