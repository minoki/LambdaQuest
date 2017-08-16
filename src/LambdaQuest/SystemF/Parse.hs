module LambdaQuest.SystemF.Parse where
import LambdaQuest.SystemF.Type
import Text.Parsec
import qualified Text.Parsec.Token as PT
import Data.List (elemIndex)
import Data.Foldable (foldl')
import Data.Void
import qualified LambdaQuest.Common.Parse as CP

type Parser = Parsec String ()

class ExtraTypeParser x where
  extraSimpleTypeExpr :: Parser (TypeT x)
  defaultType :: Parser (TypeT x)
  extraSimpleTypeExpr = parserZero
  defaultType = parserZero

instance ExtraTypeParser Void

data NameBinding = NVarBind String
                 | NAnonymousBind
                 | NTyVarBind String
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
                         , PT.reservedNames = ["if","then","else","True","False","forall","as","Int","Real","Bool","let","in"] ++ map fst builtinFunctions
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

simpleTypeExpr :: (ExtraTypeParser a) => [NameBinding] -> Parser (TypeT a)
simpleTypeExpr ctx = (reserved "Int" >> return TyInt)
                     <|> (reserved "Real" >> return TyReal)
                     <|> (reserved "Bool" >> return TyBool)
                     <|> (reserved "Unit" >> return TyUnit)
                     <|> parens (typeExpr ctx)
                     <|> (do name <- identifier <?> "type variable"
                             case NTyVarBind name `elemIndex` ctx of
                               Just i -> return (TyRef i name)
                               Nothing -> fail ("undefined type variable: " ++ name))
                     <|> extraSimpleTypeExpr
                     <?> "simple type expression"

arrTypeExpr :: (ExtraTypeParser a) => [NameBinding] -> Parser (TypeT a)
arrTypeExpr ctx = do a <- simpleTypeExpr ctx
                     (reservedOp "->" >> (TyArr a <$> arrTypeExpr (NAnonymousBind : ctx)))
                       <|> return a
                  <?> "type expression"

typeExpr :: (ExtraTypeParser a) => [NameBinding] -> Parser (TypeT a)
typeExpr ctx = forallExpr <|> arrTypeExpr ctx
               <?> "type expression"
  where forallExpr = do reserved "forall"
                        name <- identifier <?> "type variable"
                        reservedOp "."
                        t <- typeExpr (NTyVarBind name : ctx)
                        return (TyAll name t)

simpleTerm :: (ExtraTypeParser a) => [NameBinding] -> Parser (TermT a)
simpleTerm ctx = (reserved "True" >> return (TPrimValue $ PVBool True))
                 <|> (reserved "False" >> return (TPrimValue $ PVBool False))
                 <|> (either (TPrimValue . PVInt) (TPrimValue . PVReal) <$> naturalOrFloat <?> "number")
                 <|> parens (term ctx)
                 <|> choice [reserved name >> return (TPrimValue v) | (name,v) <- builtinFunctions]
                 <|> (do name <- identifier <?> "variable"
                         case NVarBind name `elemIndex` ctx of
                           Just i -> return (TRef i name)
                           Nothing -> fail ("undefined variable: " ++ name))
                 <?> "simple expression"

-- function application / type application
appTerm :: (ExtraTypeParser a) => [NameBinding] -> Parser (TermT a)
appTerm ctx = do x <- simpleTerm ctx
                 xs <- many ((flip TApp <$> simpleTerm ctx)
                              <|> (flip TTyApp <$> brackets (typeExpr ctx)))
                 return (foldl' (flip ($)) x xs)

term :: (ExtraTypeParser a) => [NameBinding] -> Parser (TermT a)
term ctx = lambdaAbstraction
           <|> typeAbstraction
           <|> letIn
           <|> ifThenElse
           <|> appTerm ctx
           <?> "expression"
  where lambdaAbstraction = do reservedOp "\\"
                               name <- identifier
                               varType <- (reservedOp ":" >> typeExpr ctx) <|> defaultType
                               reservedOp "."
                               body <- term (NVarBind name : ctx)
                               return (TAbs name varType body)
        typeAbstraction = do reservedOp "?"
                             name <- identifier
                             reservedOp "."
                             body <- term (NTyVarBind name : ctx)
                             return (TTyAbs name body)
        letIn = do reserved "let"
                   name <- identifier
                   reservedOp "="
                   definition <- term ctx
                   reserved "in"
                   body <- term (NVarBind name : ctx)
                   return $ TLet name definition body
        ifThenElse = do reserved "if"
                        cond <- term ctx
                        reserved "then"
                        thenExpr <- term ctx
                        reserved "else"
                        elseExpr <- term ctx
                        return (TIf cond thenExpr elseExpr)

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
