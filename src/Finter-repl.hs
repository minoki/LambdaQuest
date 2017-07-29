-- Read-Eval-Print loop for LambdaQuest.Finter

module Main where
import LambdaQuest.Finter
import LambdaQuest.Finter.Parse
import LambdaQuest.Finter.PrettyPrint
import LambdaQuest.Finter.Type
import LambdaQuest.Finter.TypeCheck (typeOf)
import LambdaQuest.Finter.Eval (termShift,termTypeSubst,eval1,ValueBinding(..))
import Control.Monad (when)
import System.IO
import Text.Parsec
import System.Console.Readline (readline,addHistory) -- from `readline' package

data ReplCommand = ReplEval Term
                 | ReplTermDef String Term
                 | ReplTypeDef String Type
                 | ReplNormalize Type

replCommand :: [NameBinding] -> Parser ReplCommand
replCommand ctx = termDef <|> typeDef <|> normalize <|> termEval <?> "REPL Command"
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
    normalize = do
      reserved "normalize"
      t <- typeExpr ctx
      eof
      return (ReplNormalize t)

data REPLBinding = Let String Term Type
                 | TypeDef String Type

toNameBinding :: REPLBinding -> NameBinding
toNameBinding (Let name _ _) = NVarBind name
toNameBinding (TypeDef name _) = NTyVarBind name

toBinding :: REPLBinding -> Binding
toBinding (Let name _ ty) = VarBind name ty
toBinding (TypeDef name ty) = TyVarBind name TyTop

toValueBinding :: REPLBinding -> ValueBinding
toValueBinding (Let _ v _) = ValueBind v
toValueBinding (TypeDef _ _) = TypeBind

resolveTypeAliasInTerm :: Type -> Int -> Term -> Term
resolveTypeAliasInTerm ty i = termShift 1 i . termTypeSubst ty i
resolveTypeAliasesInTerm :: [REPLBinding] -> Int -> Term -> Term
resolveTypeAliasesInTerm [] _ = id
resolveTypeAliasesInTerm (Let name m ty : xs) i = resolveTypeAliasesInTerm xs (i + 1)
resolveTypeAliasesInTerm (TypeDef name ty : xs) i = resolveTypeAliasesInTerm xs (i + 1) . resolveTypeAliasInTerm ty i

resolveTypeAliasInType :: Type -> Int -> Type -> Type
resolveTypeAliasInType ty i = typeShift 1 i . typeSubst ty i
resolveTypeAliasesInType :: [REPLBinding] -> Int -> Type -> Type
resolveTypeAliasesInType [] _ = id
resolveTypeAliasesInType (Let name m ty : xs) i = resolveTypeAliasesInType xs (i + 1)
resolveTypeAliasesInType (TypeDef name ty : xs) i = resolveTypeAliasesInType xs (i + 1) . resolveTypeAliasInType ty i

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
        Right (ReplEval tm) -> let tm' = resolveTypeAliasesInTerm ctx 0 tm
          in case typeOf (map toBinding ctx) tm' of
               Left error -> do
                 putStrLn $ "Type error: " ++ error
                 repl ctx
               Right ty -> do
                 putStr $ "Type is " ++ prettyPrintType ty ++ " ("
                 let normalized = cCanonicalToOrdinary (normalizeType (normalizeContext $ map toBinding ctx) ty)
                 putStrLn $ "canonical type is " ++ prettyPrintType normalized ++ ")."
                 putStrLn "Evaluation:"
                 putStrLn (prettyPrintTerm tm')
                 evalLoop tm'
                 repl ctx
        Right (ReplTermDef name tm) -> let tm' = resolveTypeAliasesInTerm ctx 0 tm
          in case typeOf (map toBinding ctx) tm' of
               Left error -> do
                 putStrLn $ "Type error: " ++ error
                 repl ctx
               Right ty -> do
                 putStr $ name ++ " : " ++ prettyPrintType ty ++ " ("
                 let normalized = cCanonicalToOrdinary (normalizeType (normalizeContext $ map toBinding ctx) ty)
                 putStrLn $ "canonical type: " ++ prettyPrintType normalized ++ ")."
                 putStrLn "Evaluation:"
                 putStrLn (prettyPrintTerm tm')
                 result <- evalLoop tm'
                 case result of
                   Just value -> repl (Let name value ty : ctx)
                   Nothing -> repl ctx
        Right (ReplTypeDef name ty) -> do
            putStrLn $ name ++ " := " ++ prettyPrintType ty ++ " ("
            let normalized = cCanonicalToOrdinary (normalizeType (normalizeContext $ map toBinding ctx) ty)
            putStrLn $ "canonical type: " ++ prettyPrintType normalized ++ ")."
            repl (TypeDef name ty : ctx)
        Right (ReplNormalize ty) -> let ty' = resolveTypeAliasesInType ctx 0 ty
                                        normalized = cCanonicalToOrdinary (normalizeType (normalizeContext $ map toBinding ctx) ty)
                                    in do
          putStrLn $ "Canonical type: " ++ prettyPrintType normalized
          repl ctx
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

main :: IO ()
main = do
  putStrLn "This is Finter REPL."
  putStrLn "Press Ctrl-D to exit."
  repl []
