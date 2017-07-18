-- Read-Eval-Print loop for LambdaQuest.SystemFsub

module Main where
import LambdaQuest.SystemFsub
import LambdaQuest.SystemFsub.Parse
import LambdaQuest.SystemFsub.PrettyPrint
import LambdaQuest.SystemFsub.Type
import LambdaQuest.SystemFsub.TypeCheck (typeOf)
import LambdaQuest.SystemFsub.Eval (termTypeShift,termTypeSubst,termShift,eval1)
import Control.Monad (when)
import System.IO
import Text.Parsec
import System.Console.Readline (readline,addHistory) -- from `readline' package

data ReplCommand = ReplEval Term
                 | ReplTermDef String Term
                 | ReplTypeDef String Type

replCommand :: [String] -> [String] -> Parser ReplCommand
replCommand tyctx ctx = termDef <|> typeDef <|> termEval <?> "REPL Command"
  where
    termEval = do
      whiteSpace
      t <- term tyctx ctx
      eof
      return (ReplEval t)
    termDef = do
      reserved "let"
      name <- identifier
      reservedOp "="
      t <- term tyctx ctx
      eof
      return (ReplTermDef name t)
    typeDef = do
      reserved "type"
      name <- identifier
      reservedOp "="
      t <- typeExpr tyctx
      eof
      return (ReplTypeDef name t)

repl :: [(String,Type)] -> [(String,(Term,Type))] -> IO ()
repl tyctx ctx = do
  mline <- readline "> "
  case mline of
    Nothing -> putStrLn "Bye!" -- EOF / Ctrl-D
    Just line -> do
      addHistory line
      case parse (replCommand (map fst tyctx) (map fst ctx)) "<stdin>" line of
        Left error -> do
          print error -- parse error
          repl tyctx ctx
        Right (ReplEval tm) -> case typeOf' (map (snd . snd) ctx) tm of
          Left error -> do
            putStrLn $ "Type error: " ++ error
            repl tyctx ctx
          Right ty -> do
            putStrLn $ "Type is " ++ prettyPrintType ty ++ "."
            putStrLn "Evaluation:"
            putStrLn (prettyPrintTerm tm)
            evalLoop tm
            repl tyctx ctx
        Right (ReplTermDef name tm) -> case typeOf' (map (snd . snd) ctx) tm of
          Left error -> do
            putStrLn $ "Type error: " ++ error
            repl tyctx ctx
          Right ty -> do
            putStrLn $ name ++ " : " ++ prettyPrintType ty ++ "."
            putStrLn "Evaluation:"
            putStrLn (prettyPrintTerm tm)
            result <- evalLoop tm
            let tmMap :: (String,(Term,Type)) -> (String,(Term,Type))
                tmMap (name,(m,t)) = (name,(termShift 1 0 m,t))
            case result of
              Just value -> repl tyctx ((name,(value,ty)) : map tmMap ctx)
              Nothing -> repl tyctx ctx
        Right (ReplTypeDef name ty) -> do
            putStrLn $ name ++ " := " ++ prettyPrintType ty ++ "."
            let tmMap :: (String,(Term,Type)) -> (String,(Term,Type))
                tmMap (name,(m,t)) = (name,(termTypeShift 1 0 m,t))
                tyMap :: (String,Type) -> (String,Type)
                tyMap (name,t) = (name,typeShift 1 0 t)
            repl ((name,ty) : map tyMap tyctx) (map tmMap ctx)
  where
    prettyPrintType t = prettyPrintTypeP 0 (map fst tyctx) t ""
    prettyPrintTerm t = prettyPrintTermP 0 (map fst tyctx) (map fst ctx) t ""
    evalLoop :: Term -> IO (Maybe Term)
    evalLoop t = case eval1 tctx t of
      Left error -> do putStrLn $ "Evaluation error: " ++ error
                       return Nothing
      Right t' | isValue t' -> do
                   putStrLn $ "--> " ++ prettyPrintTerm t' ++ "."
                   return (Just t')
               | otherwise -> do
                   putStrLn $ "--> " ++ prettyPrintTerm t'
                   evalLoop t'
    tctx = map (fst . snd) ctx
    typeOf' ctx tm = typeOf [] ctx $ foldr (\et -> termTypeSubst et 0) tm (map snd tyctx)

main :: IO ()
main = do
  putStrLn "This is System F REPL."
  putStrLn "Press Ctrl-D to exit."
  repl [] []
