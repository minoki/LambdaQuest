module LambdaQuest.SystemFsub.TypeCheck where
import LambdaQuest.SystemFsub.Type
import LambdaQuest.SystemFsub.Subtype

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

typeOf :: [Type] -> [Type] -> Term -> Either String Type
typeOf tyctx ctx tm = case tm of
  TPrimValue primValue -> case primValue of
    PVInt _ -> return TyInt
    PVReal _ -> return TyReal
    PVBool _ -> return TyBool
    PVBuiltinUnary f -> return $ builtinUnaryFnType f
    PVBuiltinBinary f -> return $ builtinBinaryFnType f
  TAbs name argType body -> do
    retType <- typeOf tyctx (argType : ctx) body
    return (TyArr argType retType)
  TTyAbs name bound body -> do
    let tyctx' = map (typeShift 1 0) (bound : tyctx)
    let ctx' = map (typeShift 1 0) ctx
    retType <- typeOf tyctx' ctx' body
    return (TyAll name bound retType)
  TRef i name | i < length ctx -> return (ctx !! i)
              | otherwise -> Left $ "TRef out of range"
  TApp f x -> do
    fnType <- typeOf tyctx ctx f
    actualArgType <- typeOf tyctx ctx x
    case exposeType tyctx fnType of
      TyArr expectedArgType retType | subType tyctx actualArgType expectedArgType -> return retType
                                    | otherwise -> Left ("type error (expected: " ++ show expectedArgType ++ ", got: " ++ show actualArgType ++ ")")
      _ -> Left ("invalid function application (expected function type, got: " ++ show fnType ++ ")")
  TTyApp f t -> do
    fnType <- typeOf tyctx ctx f
    case exposeType tyctx fnType of
      TyAll _name bound bodyType | subType tyctx t bound -> return (typeSubst t 0 bodyType)
                                 | otherwise -> Left ("invalid type application (" ++ show t ++ " is not a subtype of " ++ show bound ++ ")")
      _ -> Left ("invalid type application (expected forall type, got: " ++ show fnType ++ ")")
  TIf cond then_ else_ -> do
    condType <- typeOf tyctx ctx cond
    thenType <- typeOf tyctx ctx then_
    elseType <- typeOf tyctx ctx else_
    case exposeType tyctx condType of
      TyBool -> return (joinType tyctx thenType elseType)
      _ -> Left "if-then-else: conditon must be boolean"
  TCoerce x t -> do
    xt <- typeOf tyctx ctx x
    if subType tyctx xt t
      then return t
      else Left ("type coercion: the actual type " ++ show xt ++ " is not a subtype of " ++ show t)
