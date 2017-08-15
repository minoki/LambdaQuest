{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LambdaQuest.Simple.TypeRecon where
import LambdaQuest.Simple.Type
import LambdaQuest.Simple.TypeCheck (primTypeOf,typeShift)
import LambdaQuest.Simple.Parse
import LambdaQuest.Common.Type
import Control.Monad.Writer
import Control.Monad.State.Strict
import Text.Parsec hiding (State)
import qualified Data.Map as Map

data TypeHole = TypeHole deriving (Eq,Show)
type HoledType = TypeT TypeHole
type HTerm = TermT TypeHole
pattern HTyHole = TyExtra TypeHole

newtype IndexedTypeHole = IndexedTypeHole Int deriving (Eq,Show)
type UType = TypeT IndexedTypeHole
type UTerm = TermT IndexedTypeHole
type UBinding = BindingT IndexedTypeHole
pattern UTyHole i = TyExtra (IndexedTypeHole i)

newTypeHole :: (MonadState Int m) => m UType
newTypeHole = do i <- get
                 modify' (+1)
                 return $ UTyHole i

assignTypeIdTy :: (MonadState Int m) => HoledType -> m UType
assignTypeIdTy ty = case ty of
  TyPrim p -> return $ TyPrim p
  TyArr s t -> TyArr <$> assignTypeIdTy s <*> assignTypeIdTy t
  TyExtra TypeHole -> newTypeHole

assignTypeIdTm :: (MonadState Int m) => HTerm -> m UTerm
assignTypeIdTm tm = case tm of
  TPrimValue v -> return $ TPrimValue v
  TAbs name ty body -> TAbs name <$> assignTypeIdTy ty <*> assignTypeIdTm body
  TRef i name -> return $ TRef i name
  TApp f x -> TApp <$> assignTypeIdTm f <*> assignTypeIdTm x
  TLet name def body -> TLet name <$> assignTypeIdTm def <*> assignTypeIdTm body
  TIf co th el -> TIf <$> assignTypeIdTm co <*> assignTypeIdTm th <*> assignTypeIdTm el
  -- HTTyAnn tm ty -> UTyAnn <$> assignTypeIdTm tm <*> assignTypeIdTy ty

constraints :: [UBinding] -> UTerm -> WriterT [(UType,UType)] (State Int) UType
constraints ctx tm = case tm of
  TPrimValue primValue -> return (primTypeOf primValue)
  TAbs name argType body -> TyArr argType <$> constraints (VarBind name argType : ctx) body
  TRef i name -> return $ typeShift (i + 1) 0 $ getTypeFromContext ctx i
  TApp f x -> do
    fnType <- constraints ctx f
    actualArgType <- constraints ctx x
    case fnType of
      TyArr expectedArgType retType | actualArgType == expectedArgType -> return $ typeShift (-1) 0 retType
                                     | otherwise -> tell [(expectedArgType, actualArgType)] >> return retType
      _ -> do h <- newTypeHole
              tell [(fnType, TyArr actualArgType h)]
              return h
  TLet name def body -> do
    defType <- constraints ctx def
    constraints (VarBind name defType : ctx) body
  TIf cond then_ else_ -> do
    condType <- constraints ctx cond
    thenType <- constraints ctx then_
    elseType <- constraints ctx else_
    case condType of
      TyBool | thenType == elseType -> return thenType
             | otherwise -> tell [(thenType, elseType)] >> return thenType
      _ -> tell [(condType, TyBool)] >> return thenType

  -- UTyAnn x ty -> do
  --   xt <- constraints ctx x
  --   tell [(xt, ty)]
  --   return ty

collectConstraints :: [UBinding] -> HTerm -> ((UTerm,UType),[(UType,UType)])
collectConstraints ctx tm = flip evalState 0 $ runWriterT $ do
  tm' <- assignTypeIdTm tm
  ((,) tm') <$> constraints ctx tm'

freeVars :: UType -> [Int]
freeVars (TyPrim _) = []
freeVars (TyArr s t) = freeVars s ++ freeVars t
freeVars (TyExtra (IndexedTypeHole i)) = [i]

replaceHole :: Int -> UType -> UType -> UType
replaceHole i s t = case t of
  TyPrim _ -> t
  TyArr u v -> TyArr (replaceHole i s u) (replaceHole i s v)
  TyExtra (IndexedTypeHole j) | i == j -> s
                              | otherwise -> t

replaceHoleP :: Int -> UType -> (UType,UType) -> (UType,UType)
replaceHoleP i s (t0,t1) = (replaceHole i s t0, replaceHole i s t1)

unify :: [(UType,UType)] -> Map.Map Int UType -> Maybe (Map.Map Int UType)
unify [] m = return m
unify ((s,t):xs) m
  | s == t = unify xs m
  | UTyHole i <- s, i `notElem` freeVars t = unify (map (replaceHoleP i t) xs) (Map.insert i t $ replaceHole i t <$> m)
  | UTyHole i <- t, i `notElem` freeVars s = unify (map (replaceHoleP i s) xs) (Map.insert i s $ replaceHole i s <$> m)
  | TyArr s0 s1 <- s, TyArr t0 t1 <- t = unify ((s0,t0):(s1,t1):xs) m
  | otherwise = Nothing

applyUnificationResultTy :: Map.Map Int UType -> UType -> UType
applyUnificationResultTy m t = case t of
  TyPrim _ -> t
  TyArr u v -> TyArr (applyUnificationResultTy m u) (applyUnificationResultTy m v)
  TyExtra (IndexedTypeHole i) | Just r <- Map.lookup i m -> r
                              | otherwise -> t

applyUnificationResultTm :: Map.Map Int UType -> UTerm -> UTerm
applyUnificationResultTm m t = case t of
  TPrimValue _ -> t
  TAbs name ty body -> TAbs name (applyUnificationResultTy m ty) (applyUnificationResultTm m body)
  TRef _ _ -> t
  TApp s t -> TApp (applyUnificationResultTm m s) (applyUnificationResultTm m t)
  TLet name def body -> TLet name (applyUnificationResultTm m def) (applyUnificationResultTm m body)
  TIf cond then_ else_ -> TIf (applyUnificationResultTm m cond) (applyUnificationResultTm m then_) (applyUnificationResultTm m else_)
  -- UTyAnn t ty -> UTyAnn (applyUnificationResultTm m t) (applyUnificationResultTy m ty)

instance ExtraTypeParser TypeHole where
  extraSimpleTypeExpr' = reserved "_" >> return HTyHole
  defaultType' = return HTyHole

parseHoledTerm :: SourceName -> String -> Either ParseError HTerm
parseHoledTerm name input = parse wholeParser name input
  where wholeParser = do
          whiteSpace
          t <- term []
          eof
          return t

parseHoledTermAndUnify :: SourceName -> String -> Either String (UType,UTerm)
parseHoledTermAndUnify name input = case parseHoledTerm name input of
  Left err -> Left (show err)
  Right ht -> let ((utm,ut),ct) = collectConstraints [] ht
              in case unify ct Map.empty of
                   Nothing -> Left "unification failed"
                   Just m -> Right (applyUnificationResultTy m ut, applyUnificationResultTm m utm)
