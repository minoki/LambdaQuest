-- Read-Eval-Print loop for LambdaQuest.LetPoly

module Main where
import LambdaQuest.LetPoly.Type
import LambdaQuest.LetPoly.Parse
import LambdaQuest.LetPoly.PrettyPrint
import LambdaQuest.LetPoly.TypeCheck
import qualified LambdaQuest.LetPoly.TranslateF as TF
import qualified LambdaQuest.SystemF as F
import qualified LambdaQuest.SystemF.Parse as F
import qualified LambdaQuest.SystemF.PrettyPrint as F
import qualified LambdaQuest.SystemF.TypeCheck as F (typeShift,typeSubst,typeOf)
import qualified LambdaQuest.SystemF.Eval as F (termShift,termTypeSubst,eval1,ValueBinding(..))
import Control.Monad (when)
import Control.Monad.State.Strict
import Control.Monad.Except
import System.IO
import Text.Parsec
import qualified Data.Map as Map
import System.Console.Haskeline -- from `haskeline' package

data ReplCommand = ReplEval HTerm
                 | ReplTranslateF HTerm
                 -- | ReplTermDef String Term
                 -- | ReplTypeDef String Type

replCommand :: [NameBinding] -> Parser ReplCommand
replCommand ctx = translateF <|> termEval <?> "REPL Command"
  where
    translateF = do
      reserved "translateF"
      t <- term ctx
      eof
      return (ReplTranslateF t)
    termEval = do
      whiteSpace
      t <- term ctx
      eof
      return (ReplEval t)
    {-
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
    -}

data REPLBinding = Let String Term Type
                 | TypeDef String Type

{-
toNameBinding :: REPLBinding -> NameBinding
toNameBinding (Let name _ _) = NVarBind name
toNameBinding (TypeDef name _) = NTyVarBind name

toBinding :: REPLBinding -> Binding
toBinding (Let name _ ty) = VarBind name ty
toBinding (TypeDef name ty) = TyVarBind name

toValueBinding :: REPLBinding -> ValueBinding
toValueBinding (Let _ v _) = ValueBind v
toValueBinding (TypeDef _ _) = TypeBind

resolveTypeAliasInTerm :: Type -> Int -> Term -> Term
resolveTypeAliasInTerm ty i = termShift 1 i . termTypeSubst ty i
resolveTypeAliasesInTerm :: [REPLBinding] -> Int -> Term -> Term
resolveTypeAliasesInTerm [] _ = id
resolveTypeAliasesInTerm (Let name m ty : xs) i = resolveTypeAliasesInTerm xs (i + 1)
resolveTypeAliasesInTerm (TypeDef name ty : xs) i = resolveTypeAliasesInTerm xs (i + 1) . resolveTypeAliasInTerm ty i
-}

repl :: [REPLBinding] -> InputT IO ()
repl ctx = do
  mline <- getInputLine "> "
  case mline of
    Nothing -> outputStrLn "Bye!" -- EOF / Ctrl-D
    Just line -> do
      case parse (replCommand {-(map toNameBinding ctx)-} []) "<stdin>" line of
        Left error -> do
          outputStrLn $ show error -- parse error
          repl ctx
        Right (ReplEval tm) -> -- let tm' = resolveTypeAliasesInTerm ctx 0 tm
          let a = do tm' <- assignTypeIdTm tm
                     let ctx = [] :: [UBinding]
                     ty <- constraints ctx tm'
                     generalize ctx ty
              initialState = TypeInferenceState [] 0 Map.empty
          in case runState (runExceptT a) initialState of
            (Left error, st) -> do
              outputStrLn $ "Type error: " ++ error
              repl ctx
            (Right tySc, st) -> do
              outputStrLn $ "Type is " ++ prettyPrintTypeSchemeP 0 [] tySc "."
              --outputStrLn "Evaluation:"
              --outputStrLn (prettyPrintTerm tm')
              --evalLoop tm'
              repl ctx
        Right (ReplTranslateF tm) ->
          let a = do tm' <- assignTypeIdTm tm
                     let ctx = [] :: [UBinding]
                     (tm'',ty) <- TF.constraints ctx tm'
                     TF.generalizeTm ctx tm'' ty
              initialState = TypeInferenceState [] 0 Map.empty
          in case runState (runExceptT a) initialState of
            (Left error, st) -> do
              outputStrLn $ "Type error: " ++ error
              repl ctx
            (Right (tm,tySc), st) -> do
              outputStrLn $ "Type is " ++ prettyPrintTypeSchemeP 0 [] tySc "."
              outputStrLn $ F.prettyPrintTermP 0 [] tm "."
              let tm' = traverse (\_ -> Nothing) tm
              case F.typeOf [] <$> tm' of
                Nothing -> do
                  outputStrLn $ "Resulting term has a free type variable "
                Just (Left error) -> do
                  outputStrLn $ "[System F] Type error: " ++ error
                Just (Right ty) -> do
                  outputStrLn $ "[System F] Type is " ++ F.prettyPrintTypeP 0 [] ty "."
              repl ctx
        {-
        Right (ReplTermDef name tm) -> do
          outputStrLn "Not implemented yet. Sorry..."
          repl ctx
          let tm' = resolveTypeAliasesInTerm ctx 0 tm
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
          outputStrLn "Not implemented yet. Sorry..."
          repl ctx
          -- outputStrLn $ name ++ " := " ++ prettyPrintType ty ++ "."
          -- repl (TypeDef name ty : ctx)
         -}
  where
    prettyPrintType t = prettyPrintTypeP 0 {-(map toNameBinding ctx)-} [] t ""
    prettyPrintTerm t = prettyPrintTermP 0 {-(map toNameBinding ctx)-} [] t ""
    {-
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
    -}

main :: IO ()
main = runInputT defaultSettings $ do
  outputStrLn "This is LetPoly REPL."
  outputStrLn "Press Ctrl-D to exit."
  repl []
