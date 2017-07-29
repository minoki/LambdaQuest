module LambdaQuest.Finter.Eval
  (termShift
  ,termTypeSubst
  ,termSubstD
  ,termSubst
  ,ValueBinding(..)
  ,getValueFromContext
  ,eval1
  ,eval
  ) where
import LambdaQuest.Finter.Type
import LambdaQuest.Finter.TypeCheck

termSubstD :: Int -> Term -> Int -> Term -> Term
termSubstD depth s i t = case t of
  TAbs name ty body -> TAbs name (typeShift (-1) i ty) (termSubstD depth s (i + 1) body)
  TTyAbs name bound body -> TTyAbs name (typeShift (-1) i bound) (termSubstD depth s (i + 1) body)
  TRef j name | j == i -> termShift depth 0 s
              | j > i -> TRef (j - 1) name
              | otherwise -> t
  TApp u v -> TApp (termSubstD depth s i u) (termSubstD depth s i v)
  TTyApp u t -> TTyApp (termSubstD depth s i u) (typeShift (-1) i t)
  TIf cond then_ else_ -> TIf (termSubstD depth s i cond) (termSubstD depth s i then_) (termSubstD depth s i else_)
  TPrimValue _ -> t
  TCoerce x ty  -> TCoerce (termSubstD depth s i x) (typeShift (-1) i ty)
  TFor name tys body -> TFor name (map (typeShift (-1) i) tys) (termSubstD depth s (i + 1) body)

-- replaces occurrences of TRef j (j > i) with TRef (j-1), and TRef i with the given term
termSubst = termSubstD 0

intFromValue :: Term -> Either String Integer
intFromValue (TPrimValue (PVInt x)) = return x
intFromValue _ = Left "type error (expected an integer)"

realFromValue :: Term -> Either String Double
realFromValue (TPrimValue (PVInt x)) = return (fromIntegral x)
realFromValue (TPrimValue (PVReal x)) = return x
realFromValue _ = Left "type error (expected a real number)"

applyBuiltinUnaryFn :: BuiltinUnaryFn -> Term -> Either String Term
applyBuiltinUnaryFn f v = case f of
  BUnaryCommon f' -> case f' of
    BNegateInt -> (TPrimValue . PVInt . negate) <$> intFromValue v
    BNegateReal -> (TPrimValue . PVReal . negate) <$> realFromValue v
    BIntToReal -> (TPrimValue . PVReal . fromIntegral) <$> intFromValue v
  BNegate -> case v of
    TPrimValue (PVInt x) -> return (TPrimValue $ PVInt $ negate x)
    TPrimValue (PVReal x) -> return (TPrimValue $ PVReal $ negate x)
    _ -> Left "type error (expected an integer or a real number)"

applyBuiltinBinaryFn :: BuiltinBinaryFn -> Term -> Term -> Either String Term
applyBuiltinBinaryFn f u v = case f of
  BBinaryCommon f' -> case f' of
    BAddInt -> (TPrimValue . PVInt) <$> ((+) <$> intFromValue u <*> intFromValue v)
    BSubInt -> (TPrimValue . PVInt) <$> ((-) <$> intFromValue u <*> intFromValue v)
    BMulInt -> (TPrimValue . PVInt) <$> ((*) <$> intFromValue u <*> intFromValue v)
    BLtInt -> (TPrimValue . PVBool) <$> ((<) <$> intFromValue u <*> intFromValue v)
    BLeInt -> (TPrimValue . PVBool) <$> ((<=) <$> intFromValue u <*> intFromValue v)
    BEqualInt -> (TPrimValue . PVBool) <$> ((==) <$> intFromValue u <*> intFromValue v)
    BAddReal -> (TPrimValue . PVReal) <$> ((+) <$> realFromValue u <*> realFromValue v)
    BSubReal -> (TPrimValue . PVReal) <$> ((-) <$> realFromValue u <*> realFromValue v)
    BMulReal -> (TPrimValue . PVReal) <$> ((*) <$> realFromValue u <*> realFromValue v)
    BDivReal -> (TPrimValue . PVReal) <$> ((/) <$> realFromValue u <*> realFromValue v)
    BLtReal -> (TPrimValue . PVBool) <$> ((<) <$> realFromValue u <*> realFromValue v)
    BLeReal -> (TPrimValue . PVBool) <$> ((<=) <$> realFromValue u <*> realFromValue v)
    BEqualReal -> (TPrimValue . PVBool) <$> ((==) <$> realFromValue u <*> realFromValue v)
  BAdd -> case (TPrimValue . PVInt) <$> ((+) <$> intFromValue u <*> intFromValue v) of
    Left _ -> (TPrimValue . PVReal) <$> ((+) <$> realFromValue u <*> realFromValue v)
    Right w -> return w
  BSub -> case (TPrimValue . PVInt) <$> ((-) <$> intFromValue u <*> intFromValue v) of
    Left _ -> (TPrimValue . PVReal) <$> ((-) <$> realFromValue u <*> realFromValue v)
    Right w -> return w
  BMul -> case (TPrimValue . PVInt) <$> ((*) <$> intFromValue u <*> intFromValue v) of
    Left _ -> (TPrimValue . PVReal) <$> ((*) <$> realFromValue u <*> realFromValue v)
    Right w -> return w

data ValueBinding = ValueBind !Term
                  | TypeBind
                  deriving (Eq,Show)

getValueFromContext :: [ValueBinding] -> Int -> Term
getValueFromContext ctx i
  | i < length ctx = case ctx !! i of
                       ValueBind x -> x
                       b -> error ("TRef: expected a variable binding, found " ++ show b)
  | otherwise = error "TRef: index out of bounds"

eval1 :: [ValueBinding] -> Term -> Either String Term
eval1 ctx t = case t of
  TPrimValue _ -> return t
  TAbs _ _ _ -> return t
  TTyAbs _ _ _ -> return t
  TRef i _ -> return $ getValueFromContext ctx i
  TApp u v
    | isValue u && isValue v -> case u of
        TAbs _name _ty body -> return $ termSubst v 0 body -- no type checking here
        TPrimValue (PVBuiltinUnary f) -> applyBuiltinUnaryFn f v
        TPrimValue (PVBuiltinBinary _) -> return t -- partial application
        TApp (TPrimValue (PVBuiltinBinary f)) u' -> applyBuiltinBinaryFn f u' v
        _ -> Left "invalid function application (expected function type)"
    | isValue u -> TApp u <$> (eval1 ctx v)
    | otherwise -> TApp <$> (eval1 ctx u) <*> pure v
  TTyApp u ty
    | isValue u -> case u of
        TTyAbs _name _bound body -> return $ termTypeSubst ty 0 body
        _ -> Left "invalid type application (expected forall type)"
    | otherwise -> TTyApp <$> eval1 ctx u <*> pure ty
  TIf cond then_ else_
    | isValue cond -> case cond of
        TPrimValue (PVBool True) -> return then_  -- no type checking here
        TPrimValue (PVBool False) -> return else_ -- no type checking here
        _ -> Left "if-then-else: condition must be boolean"
    | otherwise -> TIf <$> (eval1 ctx cond) <*> pure then_ <*> pure else_
  TCoerce x ty
    | isValue x -> return x
    | otherwise -> TCoerce <$> eval1 ctx x <*> pure ty
  TFor name tys x
    | isValue x -> return $ termTypeSubst TyTop 0 x -- dummy type
    | otherwise -> TFor name tys <$> eval1 ctx x

eval :: [ValueBinding] -> Term -> Either String Term
eval ctx t | isValue t = return t
           | otherwise = eval1 ctx t >>= eval ctx
