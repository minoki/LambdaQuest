module LambdaQuest.SystemF.Eval where
import LambdaQuest.SystemF.Type
import LambdaQuest.SystemF.TypeCheck

-- replaces occurrences of TyRef j (j >= i) with TyRef (j + delta)
termTypeShift :: Int -> Int -> Term -> Term
termTypeShift delta i t = case t of
  TAbs name ty body -> TAbs name (typeShift delta i ty) (termTypeShift delta (i + 1) body)
  TTyAbs name body -> TTyAbs name (termTypeShift delta (i + 1) body)
  TApp u v -> TApp (termTypeShift delta i u) (termTypeShift delta i v)
  TTyApp u t -> TTyApp (termTypeShift delta i u) (typeShift delta i t)
  TIf cond thenT elseT -> TIf (termTypeShift delta i cond) (termTypeShift delta i thenT) (termTypeShift delta i elseT)
  TRef j name | j >= i -> TRef (j + delta) name
              | otherwise -> t
  TPrimValue _ -> t
-- termTypeShift 0 i t == 0

termTypeSubstD :: Int -> Type -> Int -> Term -> Term
termTypeSubstD depth s i t = case t of
  TPrimValue _ -> t
  TRef j name | j > i -> TRef (j - 1) name
              | otherwise -> t
  TAbs name ty body -> TAbs name (typeSubstD depth s i ty) (termTypeSubstD (depth + 1) s (i + 1) body)
  TTyAbs name body -> TTyAbs name (termTypeSubstD (depth + 1) s (i + 1) body)
  TApp u v -> TApp (termTypeSubstD depth s i u) (termTypeSubstD depth s i v)
  TTyApp u ty -> TTyApp (termTypeSubstD depth s i u) (typeSubstD depth s i ty)
  TIf cond then_ else_ -> TIf (termTypeSubstD depth s i cond) (termTypeSubstD depth s i then_) (termTypeSubstD depth s i else_)

-- replaces occurrences of TyRef j (j > i) with TyRef (j-1), and TyRef i with the given type
termTypeSubst = termTypeSubstD 0

-- replaces occurrences of TRef j (j >= i) with TRef (j + d)
termShift :: Int -> Int -> Term -> Term
termShift delta i t = case t of
  TAbs name ty body -> TAbs name (typeShift delta i ty) (termShift delta (i + 1) body)
  TTyAbs name body -> TTyAbs name (termShift delta (i + 1) body)
  TRef j name | j >= i -> TRef (j + delta) name
              | otherwise -> t
  TApp u v -> TApp (termShift delta i u) (termShift delta i v)
  TTyApp u t -> TTyApp (termShift delta i u) (typeShift delta i t)
  TIf cond then_ else_ -> TIf (termShift delta i cond) (termShift delta i then_) (termShift delta i else_)
  TPrimValue _ -> t
-- termShift 0 i t == t

termSubstD :: Int -> Term -> Int -> Term -> Term
termSubstD depth s i t = case t of
  TAbs name ty body -> TAbs name (typeShift (-1) i ty) (termSubstD depth s (i + 1) body)
  TTyAbs name body -> TTyAbs name (termSubstD depth s (i + 1) body)
  TRef j name | j == i -> termShift depth 0 s
              | j > i -> TRef (j - 1) name
              | otherwise -> t
  TApp u v -> TApp (termSubstD depth s i u) (termSubstD depth s i v)
  TTyApp u t -> TTyApp (termSubstD depth s i u) t
  TIf cond then_ else_ -> TIf (termSubstD depth s i cond) (termSubstD depth s i then_) (termSubstD depth s i else_)
  TPrimValue _ -> t

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
  BNegateInt -> (TPrimValue . PVInt . negate) <$> intFromValue v
  BNegateReal -> (TPrimValue . PVReal . negate) <$> realFromValue v
  BIntToReal -> (TPrimValue . PVReal . fromIntegral) <$> intFromValue v

applyBuiltinBinaryFn :: BuiltinBinaryFn -> Term -> Term -> Either String Term
applyBuiltinBinaryFn f u v = case f of
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
  TTyAbs _ _ -> return t
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
        TTyAbs _name body -> return $ termTypeSubst ty 0 body
        _ -> Left "invalid type application (expected forall type)"
    | otherwise -> TTyApp <$> eval1 ctx u <*> pure ty
  TIf cond then_ else_
    | isValue cond -> case cond of
        TPrimValue (PVBool True) -> return then_  -- no type checking here
        TPrimValue (PVBool False) -> return else_ -- no type checking here
        _ -> Left "if-then-else: condition must be boolean"
    | otherwise -> TIf <$> (eval1 ctx cond) <*> pure then_ <*> pure else_

-- typeOf ctx t == (eval1 [] t >>= typeOf ctx), if t is a closed term

eval :: [ValueBinding] -> Term -> Either String Term
eval ctx t | isValue t = return t
           | otherwise = eval1 ctx t >>= eval ctx
