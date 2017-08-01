module LambdaQuest.Finter.TypeCheck where
import LambdaQuest.Finter.Type
import LambdaQuest.Finter.Subtype
import Data.List
import Control.Monad
import Control.Arrow

-- replaces occurrences of TyRef j, TRef j (j >= i) with TyRef (j + delta), TRef (j + delta)
termShift :: Int -> Int -> Term -> Term
termShift delta i t = case t of
  TAbs name ty body -> TAbs name (typeShift delta i ty) (termShift delta (i + 1) body)
  TTyAbs name bound body -> TTyAbs name (typeShift delta i bound) (termShift delta (i + 1) body)
  TRef j name | j >= i -> TRef (j + delta) name
              | otherwise -> t
  TApp u v -> TApp (termShift delta i u) (termShift delta i v)
  TTyApp u t -> TTyApp (termShift delta i u) (typeShift delta i t)
  TIf cond then_ else_ -> TIf (termShift delta i cond) (termShift delta i then_) (termShift delta i else_)
  TPrimValue _ -> t
  TCoerce x ty -> TCoerce (termShift delta i x) (typeShift delta i ty)
  TFor name tys body -> TFor name (typeShift delta i <$> tys) (termShift delta (i + 1) body)
-- termShift 0 i t == t

termTypeSubstD :: Int -> Type -> Int -> Term -> Term
termTypeSubstD depth s i t = case t of
  TPrimValue _ -> t
  TRef j name | j > i -> TRef (j - 1) name
              | otherwise -> t
  TAbs name ty body -> TAbs name (typeSubstD depth s i ty) (termTypeSubstD (depth + 1) s (i + 1) body)
  TTyAbs name bound body -> TTyAbs name (typeSubstD depth s i bound) (termTypeSubstD (depth + 1) s (i + 1) body)
  TApp u v -> TApp (termTypeSubstD depth s i u) (termTypeSubstD depth s i v)
  TTyApp u ty -> TTyApp (termTypeSubstD depth s i u) (typeSubstD depth s i ty)
  TIf cond then_ else_ -> TIf (termTypeSubstD depth s i cond) (termTypeSubstD depth s i then_) (termTypeSubstD depth s i else_)
  TCoerce x ty -> TCoerce (termTypeSubstD depth s i x) (typeSubstD depth s i ty)
  TFor name tys body -> TFor name (typeSubstD depth s i <$> tys) (termTypeSubstD (depth + 1) s (i + 1) body)

-- replaces occurrences of TyRef j (j > i) with TyRef (j-1), and TyRef i with the given type
termTypeSubst = termTypeSubstD 0

typeOf :: [Binding] -> Term -> Either String Type
typeOf ctx tm = case tm of
  TPrimValue primValue -> return (primTypeOf primValue)
  TAbs name argType body -> do
    let argType' = normalizeType ctx argType
    TyArr argType <$> typeOf (VarBind name argType' : ctx) body
  TTyAbs name bound body -> do
    let bound' = normalizeType ctx bound
    TyAll name bound <$> typeOf (TyVarBind name bound' : ctx) body
  TRef i name -> return $ typeShift (i + 1) 0 $ getTypeFromContext ctx i
  TApp f x -> do
    fnType <- typeOf ctx f
    actualArgType <- typeOf ctx x
    case [(expectedArgType,retType) | TyArr expectedArgType retType <- exposeType ctx fnType] of
      [] -> Left ("invalid function application (expected function type, got: " ++ show fnType ++ ")")
      xs -> case [typeShift (-1) 0 retType | (expectedArgType,retType) <- xs, subType ctx actualArgType expectedArgType] of
        [] -> Left ("type error (expected: " ++ concat (intersperse ", " $ map (show . fst) xs) ++ ", got: " ++ show actualArgType ++ ")")
        [y] -> return y
        ys -> return $ TyInter ys
  TTyApp f t -> do
    fnType <- typeOf ctx f
    case [(bound,bodyType) | TyAll _name bound bodyType <- exposeType ctx fnType] of
      [] -> Left ("invalid type application (expected forall type, got: " ++ show fnType ++ ")")
      xs -> case [typeSubst t 0 bodyType | (bound,bodyType) <- xs, subType ctx t bound] of
        [] -> Left ("invalid type application (" ++ show t ++ " is not a subtype of " ++ concat (intersperse ", " $ map (show . fst) xs) ++ ")")
        [y] -> return y
        ys -> return $ TyInter ys
  TIf cond then_ else_ -> do
    condType <- typeOf ctx cond
    thenType <- typeOf ctx then_
    elseType <- typeOf ctx else_
    if any (== TyBool) $ exposeType ctx condType
      then return (joinType ctx thenType elseType)
      else Left "if-then-else: conditon must be boolean"
  TCoerce x t -> do
    xt <- typeOf ctx x
    if subType ctx xt t
      then return t
      else Left ("type coercion: the actual type " ++ show xt ++ " is not a subtype of " ++ show t)
  TFor name tys x -> do
    let gather :: [Either a b] -> Either [a] [b]
        gather (Right x:xs) = Right (x : foldr (const id ||| (:)) [] xs)
        gather (Left err:xs) = left (err:) $ gather xs
        gather [] = Left []
    case gather [typeOf ctx (termTypeSubst ty 0 x) | ty <- tys] of
      Left errors -> Left $ "type alternation failed: " ++ concat (intersperse ", " errors)
      Right [] -> error "internal error in handling type alternation"
      Right [y] -> return y
      Right ys -> return $ TyInter ys
