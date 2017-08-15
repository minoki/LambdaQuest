{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module LambdaQuest.Simple.Parse where
import LambdaQuest.Simple.Type
import Text.Parsec
import qualified Text.Parsec.Token as PT
import Data.List (elemIndex)
import Data.Foldable (foldl')
import Data.Void
import qualified LambdaQuest.Common.Parse as CP

type Parser = Parsec String ()

data NameBinding = NVarBind String
                 | NAnonymousBind
                 deriving (Eq,Show)

builtinFunctions :: [(String,PrimValue)]
builtinFunctions = [("negateInt",PVBuiltinUnary BNegateInt)
                   ,("negateReal",PVBuiltinUnary BNegateReal)
                   ,("intToReal",PVBuiltinUnary BIntToReal)
                   ,("addInt",PVBuiltinBinary BAddInt)
                   ,("subInt",PVBuiltinBinary BSubInt)
                   ,("mulInt",PVBuiltinBinary BMulInt)
                   ,("ltInt",PVBuiltinBinary BLtInt)
                   ,("leInt",PVBuiltinBinary BLeInt)
                   ,("equalInt",PVBuiltinBinary BEqualInt)
                   ,("addReal",PVBuiltinBinary BAddReal)
                   ,("subReal",PVBuiltinBinary BSubReal)
                   ,("mulReal",PVBuiltinBinary BMulReal)
                   ,("divReal",PVBuiltinBinary BDivReal)
                   ,("ltReal",PVBuiltinBinary BLtReal)
                   ,("leReal",PVBuiltinBinary BLeReal)
                   ,("equalReal",PVBuiltinBinary BEqualReal)
                   ,("unit",PVUnit)
                   ]

langDef = PT.LanguageDef { PT.commentStart = ""
                         , PT.commentEnd = ""
                         , PT.commentLine = ""
                         , PT.nestedComments = False
                         , PT.identStart = letter <|> char '_'
                         , PT.identLetter = alphaNum <|> char '_'
                         , PT.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                         , PT.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                         , PT.reservedNames = ["if","then","else","True","False","forall","as","Int","Real","Bool","let","in","_"] ++ map fst builtinFunctions
                         , PT.reservedOpNames = []
                         , PT.caseSensitive = True
                         }
tokenParser = PT.makeTokenParser langDef
identifier = PT.identifier tokenParser :: Parser String
reserved = PT.reserved tokenParser :: String -> Parser ()
operator = PT.operator tokenParser :: Parser String
reservedOp = PT.reservedOp tokenParser :: String -> Parser ()
naturalOrFloat = CP.naturalOrFloat tokenParser :: Parser (Either Integer Double)
symbol = PT.symbol tokenParser :: String -> Parser String
parens = PT.parens tokenParser
braces = PT.braces tokenParser
brackets = PT.brackets tokenParser
whiteSpace = PT.whiteSpace tokenParser

class ExtraTypeParser x where
  extraSimpleTypeExpr' :: Parser (TypeT x)
  defaultType' :: Parser (TypeT x)

instance ExtraTypeParser Void where
  extraSimpleTypeExpr' = parserZero
  defaultType' = parserZero

class GType ty => TypeParser ty where
  extraSimpleTypeExpr :: Parser ty
  defaultType :: Parser ty

instance (ExtraTypeParser x) => TypeParser (TypeT x)  where
  extraSimpleTypeExpr = extraSimpleTypeExpr'
  defaultType = defaultType'

class TypeParser ty => TermParser ty tm | tm -> ty where
  tPrimValue :: PrimValue -> tm
  tAbs :: String -> ty -> tm -> tm
  tRef :: Int -> String -> tm
  tApp :: tm -> tm -> tm
  tLet :: String -> tm -> tm -> tm
  tIf :: tm -> tm -> tm -> tm

instance (ExtraTypeParser x) => TermParser (TypeT x) (TermT x) where
  tPrimValue = TPrimValue
  tAbs = TAbs
  tRef = TRef
  tApp = TApp
  tLet = TLet
  tIf = TIf

simpleTypeExpr :: (TypeParser ty) => [NameBinding] -> Parser ty
simpleTypeExpr ctx = (reserved "Int" >> return tyInt)
                     <|> (reserved "Real" >> return tyReal)
                     <|> (reserved "Bool" >> return tyBool)
                     <|> (reserved "Unit" >> return tyUnit)
                     <|> parens (typeExpr ctx)
                     <|> extraSimpleTypeExpr
                     <?> "simple type expression"

arrTypeExpr :: (TypeParser ty) => [NameBinding] -> Parser ty
arrTypeExpr ctx = do a <- simpleTypeExpr ctx
                     (reservedOp "->" >> (tyArr a <$> arrTypeExpr (NAnonymousBind : ctx)))
                       <|> return a
                  <?> "type expression"

typeExpr :: (TypeParser ty) => [NameBinding] -> Parser ty
typeExpr ctx = arrTypeExpr ctx
               <?> "type expression"

simpleTerm :: (TermParser ty tm) => [NameBinding] -> Parser tm
simpleTerm ctx = (reserved "True" >> return (tPrimValue $ PVBool True))
                 <|> (reserved "False" >> return (tPrimValue $ PVBool False))
                 <|> (either (tPrimValue . PVInt) (tPrimValue . PVReal) <$> naturalOrFloat <?> "number")
                 <|> parens (term ctx)
                 <|> choice [reserved name >> return (tPrimValue v) | (name,v) <- builtinFunctions]
                 <|> (do name <- identifier <?> "variable"
                         case NVarBind name `elemIndex` ctx of
                           Just i -> return (tRef i name)
                           Nothing -> fail ("undefined variable: " ++ name))
                 <?> "simple expression"

-- function application
appTerm :: (TermParser ty tm) => [NameBinding] -> Parser tm
appTerm ctx = do x <- simpleTerm ctx
                 xs <- many (flip tApp <$> simpleTerm ctx)
                 return (foldl' (flip ($)) x xs)

term :: (TermParser ty tm) => [NameBinding] -> Parser tm
term ctx = lambdaAbstraction
           <|> ifThenElse
           <|> letIn
           <|> appTerm ctx
           <?> "expression"
  where lambdaAbstraction = do reservedOp "\\"
                               name <- identifier
                               varType <- (reservedOp ":" >> typeExpr ctx) <|> defaultType
                               reservedOp "."
                               body <- term (NVarBind name : ctx)
                               return (tAbs name varType body)
        letIn = do reserved "let"
                   name <- identifier
                   reservedOp "="
                   definition <- term ctx
                   reserved "in"
                   body <- term (NVarBind name : ctx)
                   return $ tLet name definition body
        ifThenElse = do reserved "if"
                        cond <- term ctx
                        reserved "then"
                        thenExpr <- term ctx
                        reserved "else"
                        elseExpr <- term ctx
                        return (tIf cond thenExpr elseExpr)

parseType :: SourceName -> String -> Either ParseError Type
parseType name input = parse wholeParser name input
  where wholeParser = do
          whiteSpace
          t <- typeExpr []
          eof
          return t

parseTerm :: SourceName -> String -> Either ParseError Term
parseTerm name input = parse wholeParser name input
  where wholeParser = do
          whiteSpace
          t <- term []
          eof
          return t
