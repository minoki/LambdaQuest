-- Read-Eval-Print loop for LambdaQuest.SystemFsub

module Main where
import LambdaQuest.SystemFsub
import LambdaQuest.SystemFsub.Parse
import LambdaQuest.SystemFsub.PrettyPrint
import LambdaQuest.SystemFsub.Type
import LambdaQuest.SystemFsub.TypeCheck (typeOf)
import LambdaQuest.SystemFsub.Eval (termShift,termTypeSubst,eval1,ValueBinding(..))
import qualified LambdaQuest.SystemFsub.Coercion as C
import qualified LambdaQuest.SystemF as F
import qualified LambdaQuest.SystemF.Parse as F
import Control.Monad (when)
import System.IO
import Text.Parsec
import System.Console.Haskeline -- from `haskeline' package

data ReplCommand = ReplEval Term
                 | ReplTermDef String Term
                 | ReplTypeDef String Type
                 | ReplTranslate Term

replCommand :: [NameBinding] -> Parser ReplCommand
replCommand ctx = termDef <|> typeDef <|> translate <|> termEval <?> "REPL Command"
  where
    termEval = do
      whiteSpace
      t <- term ctx
      eof
      return (ReplEval t)
    termDef = try $ do
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
    translate = do
      reserved "translate"
      t <- term ctx
      eof
      return (ReplTranslate t)

data REPLBinding = Let String Term Type
                 | TypeDef String Type

toNameBinding :: REPLBinding -> NameBinding
toNameBinding (Let name _ _) = NVarBind name
toNameBinding (TypeDef name _) = NTyVarBind name

toNameBindingF :: REPLBinding -> F.NameBinding
toNameBindingF (Let name _ _) = F.NVarBind name
toNameBindingF (TypeDef name _) = F.NTyVarBind name

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

repl :: [REPLBinding] -> InputT IO ()
repl ctx = do
  mline <- getInputLine "> "
  case mline of
    Nothing -> outputStrLn "Bye!" -- EOF / Ctrl-D
    Just line -> do
      case parse (replCommand (map toNameBinding ctx)) "<stdin>" line of
        Left error -> do
          outputStrLn $ show error -- parse error
          repl ctx
        Right (ReplEval tm) -> let tm' = resolveTypeAliasesInTerm ctx 0 tm
          in case typeOf (map toBinding ctx) tm' of
               Left error -> do
                 outputStrLn $ "Type error: " ++ error
                 repl ctx
               Right ty -> do
                 outputStrLn $ "Type is " ++ prettyPrintType ty ++ "."
                 outputStrLn "Evaluation:"
                 outputStrLn (prettyPrintTerm tm')
                 evalLoop tm'
                 repl ctx
        Right (ReplTermDef name tm) -> let tm' = resolveTypeAliasesInTerm ctx 0 tm
          in case typeOf (map toBinding ctx) tm' of
               Left error -> do
                 outputStrLn $ "Type error: " ++ error
                 repl ctx
               Right ty -> do
                 outputStrLn $ name ++ " : " ++ prettyPrintType ty ++ "."
                 outputStrLn "Evaluation:"
                 outputStrLn (prettyPrintTerm tm')
                 result <- evalLoop tm'
                 case result of
                   Just value -> repl (Let name value ty : ctx)
                   Nothing -> repl ctx
        Right (ReplTypeDef name ty) -> do
            outputStrLn $ name ++ " := " ++ prettyPrintType ty ++ "."
            repl (TypeDef name ty : ctx)
        Right (ReplTranslate tm) -> let tm' = resolveTypeAliasesInTerm ctx 0 tm
          in case C.mapTerm (map toBinding ctx) tm' of
               Left error -> do
                 outputStrLn $ "Type error: " ++ error
                 repl ctx
               Right (tr, ty) -> do
                 outputStrLn $ "[System Fsub] Type is " ++ prettyPrintType ty ++ "."
                 outputStrLn $ "Translation to System F: " ++ F.prettyPrintTermP 0 (map toNameBindingF ctx) tr "" ++ "."
                 let bf (VarBind n ty : ctx) = F.VarBind n (C.mapType ctx ty) : bf ctx
                     bf (TyVarBind n _ : ctx) = F.TyVarBind n : bf ctx
                     bf (AnonymousBind : ctx) = F.AnonymousBind : bf ctx
                     bf [] = []
                     expectedType = C.mapType (map toBinding ctx) ty
                 case F.typeOf (bf $ map toBinding ctx) tr of
                   Left error -> do
                     outputStrLn $ "[System F] Type error: " ++ error
                     outputStrLn $ "[System F] (Type should be " ++ F.prettyPrintTypeP 0 (map toNameBindingF ctx) expectedType "" ++ ".)"
                   Right ty' | ty' == expectedType -> do
                                 outputStrLn $ "[System F] Type is " ++ F.prettyPrintTypeP 0 (map toNameBindingF ctx) ty' "" ++ "."
                             | otherwise -> do
                                 outputStrLn $ "Type mismatch between the translator and System F's type checker:"
                                 outputStrLn $ "[Translator] " ++ F.prettyPrintTypeP 0 (map toNameBindingF ctx) expectedType ""
                                 outputStrLn $ "[System F type checker] " ++ F.prettyPrintTypeP 0 (map toNameBindingF ctx) ty' ""
                 repl ctx
  where
    prettyPrintType t = prettyPrintTypeP 0 (map toNameBinding ctx) t ""
    prettyPrintTerm t = prettyPrintTermP 0 (map toNameBinding ctx) t ""
    evalLoop :: Term -> InputT IO (Maybe Term)
    evalLoop t = case eval1 (map toValueBinding ctx) t of
      Left error -> do outputStrLn $ "Evaluation error: " ++ error
                       return Nothing
      Right t' | isValue t' -> do
                   outputStrLn $ "--> " ++ prettyPrintTerm t' ++ "."
                   return (Just t')
               | otherwise -> do
                   outputStrLn $ "--> " ++ prettyPrintTerm t'
                   evalLoop t'

main :: IO ()
main = runInputT defaultSettings $ do
  outputStrLn "This is System Fsub REPL."
  outputStrLn "Press Ctrl-D to exit."
  repl []
