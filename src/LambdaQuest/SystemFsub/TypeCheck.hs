module LambdaQuest.SystemFsub.TypeCheck where
import LambdaQuest.SystemFsub.Type
import LambdaQuest.SystemFsub.Subtype
import LambdaQuest.Common.Type

primTypeOf :: PrimValue -> Type
primTypeOf = genPrimTypeOf TyPrim TyArr

typeOf :: [Binding] -> Term -> Either String Type
typeOf ctx tm = case tm of
  TPrimValue primValue -> return (primTypeOf primValue)
  TAbs name argType body -> TyArr argType <$> typeOf (VarBind name argType : ctx) body
  TTyAbs name bound body -> TyAll name bound <$> typeOf (TyVarBind name bound : ctx) body
  TRef i name -> return $ typeShift (i + 1) 0 $ getTypeFromContext ctx i
  TApp f x -> do
    fnType <- typeOf ctx f
    actualArgType <- typeOf ctx x
    case exposeType ctx fnType of
      TyArr expectedArgType retType | subType ctx actualArgType expectedArgType -> return $ typeShift (-1) 0 retType
                                    | otherwise -> Left ("type error (expected: " ++ show expectedArgType ++ ", got: " ++ show actualArgType ++ ")")
      _ -> Left ("invalid function application (expected function type, got: " ++ show fnType ++ ")")
  TTyApp f t -> do
    fnType <- typeOf ctx f
    case exposeType ctx fnType of
      TyAll _name bound bodyType | subType ctx t bound -> return (typeSubst t 0 bodyType)
                                 | otherwise -> Left ("invalid type application (" ++ show t ++ " is not a subtype of " ++ show bound ++ ")")
      _ -> Left ("invalid type application (expected forall type, got: " ++ show fnType ++ ")")
  TLet name def body -> do
    definedType <- typeOf ctx def
    typeOf (VarBind name definedType : ctx) body
  TIf cond then_ else_ -> do
    condType <- typeOf ctx cond
    thenType <- typeOf ctx then_
    elseType <- typeOf ctx else_
    case exposeType ctx condType of
      TyBool -> return (joinType ctx thenType elseType)
      _ -> Left "if-then-else: conditon must be boolean"
  TCoerce x t -> do
    xt <- typeOf ctx x
    if subType ctx xt t
      then return t
      else Left ("type coercion: the actual type " ++ show xt ++ " is not a subtype of " ++ show t)
