{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LambdaQuest.Simple.TypeRecon where
import LambdaQuest.Simple.Type
import LambdaQuest.Simple.Parse
import LambdaQuest.Common.Type
import Control.Monad.Writer
import Control.Monad.State.Strict
import Text.Parsec hiding (State)
import qualified Data.Map as Map

data HoledType = HTyPrim !PrimType
               | HTyArr HoledType HoledType
               | HTyHole
               deriving (Show)

data HTerm = HTPrimValue !PrimValue       -- primitive value
           | HTAbs String HoledType HTerm -- lambda abstraction
           | HTRef !Int String            -- variable (de Bruijn index)
           | HTApp HTerm HTerm            -- function application
           | HTLet String HTerm HTerm     -- let-in
           | HTIf HTerm HTerm HTerm       -- if-then-else
           | HTTyAnn HTerm HoledType      -- type annotation
           deriving (Show)

data UType = UTyPrim !PrimType
           | UTyArr UType UType
           | UTyHole !Int
           deriving (Eq,Show)

data UTerm = UTPrimValue !PrimValue   -- primitive value
           | UTAbs String UType UTerm -- lambda abstraction
           | UTRef !Int String        -- variable (de Bruijn index)
           | UTApp UTerm UTerm        -- function application
           | UTLet String UTerm UTerm -- let-in
           | UTIf UTerm UTerm UTerm   -- if-then-else
           | UTyAnn UTerm UType
           deriving (Show)

data UBinding = UVarBind String UType -- variable binding (name, type)
              | UAnonymousBind        -- placeholder for function type
              deriving (Eq,Show)

getTypeFromContextU :: [UBinding] -> Int -> UType
getTypeFromContextU ctx i
  | i < length ctx = case ctx !! i of
                       UVarBind _ t -> t
                       b -> error ("TRef: expected a variable binding, found " ++ show b)
  | otherwise = error "TRef: index out of bounds"

newTypeHole :: (MonadState Int m) => m UType
newTypeHole = do i <- get
                 modify' (+1)
                 return $ UTyHole i

assignTypeIdTy :: (MonadState Int m) => HoledType -> m UType
assignTypeIdTy ty = case ty of
  HTyPrim p -> return $ UTyPrim p
  HTyArr s t -> UTyArr <$> assignTypeIdTy s <*> assignTypeIdTy t
  HTyHole -> newTypeHole

assignTypeIdTm :: (MonadState Int m) => HTerm -> m UTerm
assignTypeIdTm tm = case tm of
  HTPrimValue v -> return $ UTPrimValue v
  HTAbs name ty body -> UTAbs name <$> assignTypeIdTy ty <*> assignTypeIdTm body
  HTRef i name -> return $ UTRef i name
  HTApp f x -> UTApp <$> assignTypeIdTm f <*> assignTypeIdTm x
  HTLet name def body -> UTLet name <$> assignTypeIdTm def <*> assignTypeIdTm body
  HTIf co th el -> UTIf <$> assignTypeIdTm co <*> assignTypeIdTm th <*> assignTypeIdTm el
  HTTyAnn tm ty -> UTyAnn <$> assignTypeIdTm tm <*> assignTypeIdTy ty

uTypeShift :: Int -> Int -> UType -> UType
uTypeShift d i t = t

primTypeOfU :: PrimValue -> UType
primTypeOfU = genPrimTypeOf UTyPrim UTyArr

constraints :: [UBinding] -> UTerm -> WriterT [(UType,UType)] (State Int) UType
constraints ctx tm = case tm of
  UTPrimValue primValue -> return (primTypeOfU primValue)
  UTAbs name argType body -> UTyArr argType <$> constraints (UVarBind name argType : ctx) body
  UTRef i name -> return $ uTypeShift (i + 1) 0 $ getTypeFromContextU ctx i
  UTApp f x -> do
    fnType <- constraints ctx f
    actualArgType <- constraints ctx x
    case fnType of
      UTyArr expectedArgType retType | actualArgType == expectedArgType -> return $ uTypeShift (-1) 0 retType
                                     | otherwise -> tell [(expectedArgType, actualArgType)] >> return retType
      _ -> do h <- newTypeHole
              tell [(fnType, UTyArr actualArgType h)]
              return h
  UTLet name def body -> do
    defType <- constraints ctx def
    constraints (UVarBind name defType : ctx) body
  UTIf cond then_ else_ -> do
    condType <- constraints ctx cond
    thenType <- constraints ctx then_
    elseType <- constraints ctx else_
    case condType of
      UTyPrim PTyBool | thenType == elseType -> return thenType
                      | otherwise -> tell [(thenType, elseType)] >> return thenType
      _ -> tell [(condType, UTyPrim PTyBool)] >> return thenType

  UTyAnn x ty -> do
    xt <- constraints ctx x
    tell [(xt, ty)]
    return ty

collectConstraints :: [UBinding] -> HTerm -> ((UTerm,UType),[(UType,UType)])
collectConstraints ctx tm = flip evalState 0 $ runWriterT $ do
  tm' <- assignTypeIdTm tm
  ((,) tm') <$> constraints ctx tm'

freeVars :: UType -> [Int]
freeVars (UTyPrim _) = []
freeVars (UTyArr s t) = freeVars s ++ freeVars t
freeVars (UTyHole i) = [i]

replaceHole :: Int -> UType -> UType -> UType
replaceHole i s t = case t of
  UTyPrim _ -> t
  UTyArr u v -> UTyArr (replaceHole i s u) (replaceHole i s v)
  UTyHole j | i == j -> s
            | otherwise -> t

replaceHoleP :: Int -> UType -> (UType,UType) -> (UType,UType)
replaceHoleP i s (t0,t1) = (replaceHole i s t0, replaceHole i s t1)

unify :: [(UType,UType)] -> Map.Map Int UType -> Maybe (Map.Map Int UType)
unify [] m = return m
unify ((s,t):xs) m
  | s == t = unify xs m
  | UTyHole i <- s, i `notElem` freeVars t = unify (map (replaceHoleP i t) xs) (Map.insert i t $ replaceHole i t <$> m)
  | UTyHole i <- t, i `notElem` freeVars s = unify (map (replaceHoleP i s) xs) (Map.insert i s $ replaceHole i s <$> m)
  | UTyArr s0 s1 <- s, UTyArr t0 t1 <- t = unify ((s0,t0):(s1,t1):xs) m
  | otherwise = Nothing

applyUnificationResultTy :: Map.Map Int UType -> UType -> UType
applyUnificationResultTy m t = case t of
  UTyPrim _ -> t
  UTyArr u v -> UTyArr (applyUnificationResultTy m u) (applyUnificationResultTy m v)
  UTyHole i | Just r <- Map.lookup i m -> r
            | otherwise -> t

applyUnificationResultTm :: Map.Map Int UType -> UTerm -> UTerm
applyUnificationResultTm m t = case t of
  UTPrimValue _ -> t
  UTAbs name ty body -> UTAbs name (applyUnificationResultTy m ty) (applyUnificationResultTm m body)
  UTRef _ _ -> t
  UTApp s t -> UTApp (applyUnificationResultTm m s) (applyUnificationResultTm m t)
  UTLet name def body -> UTLet name (applyUnificationResultTm m def) (applyUnificationResultTm m body)
  UTIf cond then_ else_ -> UTIf (applyUnificationResultTm m cond) (applyUnificationResultTm m then_) (applyUnificationResultTm m else_)
  UTyAnn t ty -> UTyAnn (applyUnificationResultTm m t) (applyUnificationResultTy m ty)

instance GType HoledType where
  tyPrim = HTyPrim
  tyArr = HTyArr

instance TypeParser HoledType where
  extraSimpleTypeExpr = reserved "_" >> return HTyHole
  defaultType = return HTyHole

instance TermParser HoledType HTerm where
  tPrimValue = HTPrimValue
  tAbs = HTAbs
  tRef = HTRef
  tApp = HTApp
  tLet = HTLet
  tIf = HTIf

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
