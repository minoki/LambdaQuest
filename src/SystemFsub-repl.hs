-- Read-Eval-Print loop for LambdaQuest.SystemFsub

module Main where
import LambdaQuest.SystemFsub
import LambdaQuest.SystemFsub.Parse
import LambdaQuest.SystemFsub.PrettyPrint
import LambdaQuest.SystemFsub.Type
import LambdaQuest.SystemFsub.TypeCheck (typeOf)
import LambdaQuest.SystemFsub.Eval (termTypeShift,termTypeSubst,termShift,eval1,ValueBinding(..))
import Control.Monad (when)
import System.IO
import Text.Parsec
import System.Console.Readline (readline,addHistory) -- from `readline' package

data ReplCommand = ReplEval Term
                 | ReplTermDef String Term
                 | ReplTypeDef String Type

replCommand :: [NameBinding] -> Parser ReplCommand
replCommand ctx = termDef <|> typeDef <|> termEval <?> "REPL Command"
  where
    termEval = do
      whiteSpace
      t <- term ctx
      eof
      return (ReplEval t)
    termDef = do
      reserved "let"
      name <- identifier
      reservedOp "="
      t <- term ctx
      eof
      return (ReplTermDef name t)
    typeDef = do
      reserved "type"
      name <- identifier
      reservedOp "="
      t <- typeExpr ctx
      eof
      return (ReplTypeDef name t)

data REPLBinding = Let String Term Type
                 | TypeDef String Type

toNameBinding :: REPLBinding -> NameBinding
toNameBinding (Let name _ _) = NVarBind name
toNameBinding (TypeDef name _) = NTyVarBind name

toBinding :: REPLBinding -> Binding
toBinding (Let name _ ty) = VarBind name ty
toBinding (TypeDef name ty) = let bound = ty in TyVarBind name bound

toValueBinding :: REPLBinding -> ValueBinding
toValueBinding (Let _ v _) = ValueBind v
toValueBinding (TypeDef _ _) = TypeBind

repl :: [REPLBinding] -> IO ()
repl ctx = do
  mline <- readline "> "
  case mline of
    Nothing -> putStrLn "Bye!" -- EOF / Ctrl-D
    Just line -> do
      addHistory line
      case parse (replCommand (map toNameBinding ctx)) "<stdin>" line of
        Left error -> do
          print error -- parse error
          repl ctx
        Right (ReplEval tm) -> case typeOf' (map toBinding ctx) tm of
          Left error -> do
            putStrLn $ "Type error: " ++ error
            repl ctx
          Right ty -> do
            putStrLn $ "Type is " ++ prettyPrintType ty ++ "."
            putStrLn "Evaluation:"
            putStrLn (prettyPrintTerm tm)
            evalLoop tm
            repl ctx
        Right (ReplTermDef name tm) -> case typeOf' (map toBinding ctx) tm of
          Left error -> do
            putStrLn $ "Type error: " ++ error
            repl ctx
          Right ty -> do
            putStrLn $ name ++ " : " ++ prettyPrintType ty ++ "."
            putStrLn "Evaluation:"
            putStrLn (prettyPrintTerm tm)
            result <- evalLoop tm
            let bMap :: REPLBinding -> REPLBinding
                bMap (Let name m t) = Let name (termShift 1 0 m) t
                bMap x = x
            case result of
              Just value -> repl (Let name value ty : map bMap ctx)
              Nothing -> repl ctx
        Right (ReplTypeDef name ty) -> do
            putStrLn $ name ++ " := " ++ prettyPrintType ty ++ "."
            let bMap :: REPLBinding -> REPLBinding
                bMap (Let name m t) = Let name (termTypeShift 1 0 m) t
                bMap (TypeDef name t) = TypeDef name (typeShift 1 0 t)
            repl (TypeDef name ty : map bMap ctx)
  where
    prettyPrintType t = prettyPrintTypeP 0 (map toNameBinding ctx) t ""
    prettyPrintTerm t = prettyPrintTermP 0 (map toNameBinding ctx) t ""
    evalLoop :: Term -> IO (Maybe Term)
    evalLoop t = case eval1 (map toValueBinding ctx) t of
      Left error -> do putStrLn $ "Evaluation error: " ++ error
                       return Nothing
      Right t' | isValue t' -> do
                   putStrLn $ "--> " ++ prettyPrintTerm t' ++ "."
                   return (Just t')
               | otherwise -> do
                   putStrLn $ "--> " ++ prettyPrintTerm t'
                   evalLoop t'
    typeOf' ctx tm = typeOf ctx tm -- ctx $ foldr (\et -> termTypeSubst et 0) tm (map snd tyctx)

main :: IO ()
main = do
  putStrLn "This is System F REPL."
  putStrLn "Press Ctrl-D to exit."
  repl []
