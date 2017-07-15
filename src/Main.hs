-- Read-Eval-Print loop for LambdaQuest.SystemF

module Main where
import LambdaQuest.SystemF
import Control.Monad (when)
import System.IO
import System.Console.Readline (readline,addHistory) -- from `readline' package

evalLoop :: [Term] -> Term -> IO ()
evalLoop ctx t
  | isValue t = putStrLn $ "--> " ++ prettyPrintTerm t ++ "."
  | otherwise = case eval1 ctx t of
      Left error -> putStrLn $ "Evaluation error: " ++ error
      Right t' | isValue t' -> do
                   putStrLn $ "--> " ++ prettyPrintTerm t' ++ "."
               | otherwise -> do
                   putStrLn $ "--> " ++ prettyPrintTerm t'
                   evalLoop ctx t'

repl :: IO ()
repl = do
  mline <- readline "> "
  case mline of
    Nothing -> putStrLn "Bye!" -- EOF / Ctrl-D
    Just line -> do
      addHistory line
      case parseTerm "<stdin>" line of
        Left error -> do
          print error -- parse error
          repl
        Right term -> case typeOf [] term of
          Right ty -> do
            putStrLn $ "Type is " ++ prettyPrintType ty ++ "."
            putStrLn "Evaluation:"
            putStrLn (prettyPrintTerm term)
            evalLoop [] term
            repl
          Left error -> do
            putStrLn $ "Type error: " ++ error
            repl

main :: IO ()
main = do
  putStrLn "This is System F REPL."
  putStrLn "Press Ctrl-D to exit."
  repl
