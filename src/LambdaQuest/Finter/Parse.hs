module LambdaQuest.Finter.Parse where
import LambdaQuest.Finter.Type
import Text.Parsec
import qualified Text.Parsec.Token as PT
import Data.List (elemIndex)
import Data.Foldable (foldl')

type Parser = Parsec String ()

data NameBinding = NVarBind String
                 | NAnonymousBind
                 | NTyVarBind String
                 deriving (Eq,Show)

builtinFunctions :: [(String,PrimValue)]
builtinFunctions = [("negateInt",PVBuiltinUnary $ BUnaryCommon BNegateInt)
                   ,("negateReal",PVBuiltinUnary $ BUnaryCommon BNegateReal)
                   ,("intToReal",PVBuiltinUnary $ BUnaryCommon BIntToReal)
                   ,("addInt",PVBuiltinBinary $ BBinaryCommon BAddInt)
                   ,("subInt",PVBuiltinBinary $ BBinaryCommon BSubInt)
                   ,("mulInt",PVBuiltinBinary $ BBinaryCommon BMulInt)
                   ,("ltInt",PVBuiltinBinary $ BBinaryCommon BLtInt)
                   ,("leInt",PVBuiltinBinary $ BBinaryCommon BLeInt)
                   ,("equalInt",PVBuiltinBinary $ BBinaryCommon BEqualInt)
                   ,("addReal",PVBuiltinBinary $ BBinaryCommon BAddReal)
                   ,("subReal",PVBuiltinBinary $ BBinaryCommon BSubReal)
                   ,("mulReal",PVBuiltinBinary $ BBinaryCommon BMulReal)
                   ,("divReal",PVBuiltinBinary $ BBinaryCommon BDivReal)
                   ,("ltReal",PVBuiltinBinary $ BBinaryCommon BLtReal)
                   ,("leReal",PVBuiltinBinary $ BBinaryCommon BLeReal)
                   ,("equalReal",PVBuiltinBinary $ BBinaryCommon BEqualReal)
                   ,("unit",PVUnit)
                   ,("negate",PVBuiltinUnary BNegate)
                   ,("add",PVBuiltinBinary BAdd)
                   ,("sub",PVBuiltinBinary BSub)
                   ,("mul",PVBuiltinBinary BMul)
                   ]

langDef = PT.LanguageDef { PT.commentStart = ""
                         , PT.commentEnd = ""
                         , PT.commentLine = ""
                         , PT.nestedComments = False
                         , PT.identStart = letter <|> char '_'
                         , PT.identLetter = alphaNum <|> char '_'
                         , PT.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                         , PT.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                         , PT.reservedNames = ["if","then","else","True","False","forall","as","Int","Real","Bool","let","in","for"] ++ map fst builtinFunctions
                         , PT.reservedOpNames = []
                         , PT.caseSensitive = True
                         }
tokenParser = PT.makeTokenParser langDef
identifier = PT.identifier tokenParser :: Parser String
reserved = PT.reserved tokenParser :: String -> Parser ()
operator = PT.operator tokenParser :: Parser String
reservedOp = PT.reservedOp tokenParser :: String -> Parser ()
natural = PT.natural tokenParser :: Parser Integer
integer = PT.integer tokenParser :: Parser Integer
float = PT.float tokenParser :: Parser Double
symbol = PT.symbol tokenParser :: String -> Parser String
parens = PT.parens tokenParser
braces = PT.braces tokenParser
brackets = PT.brackets tokenParser
whiteSpace = PT.whiteSpace tokenParser

simpleTypeExpr :: [NameBinding] -> Parser Type
simpleTypeExpr ctx = (reserved "Int" >> return TyInt)
                     <|> (reserved "Real" >> return TyReal)
                     <|> (reserved "Bool" >> return TyBool)
                     <|> (reserved "Unit" >> return TyUnit)
                     <|> (reserved "Top" >> return TyTop)
                     <|> parens (typeExpr ctx)
                     <|> (do name <- identifier <?> "type variable"
                             case NTyVarBind name `elemIndex` ctx of
                               Just i -> return (TyRef i name)
                               Nothing -> fail ("undefined type variable: " ++ name))
                       <?> "simple type expression"

arrTypeExpr :: [NameBinding] -> Parser Type
arrTypeExpr ctx = do a <- simpleTypeExpr ctx
                     ((reservedOp "->" >> (TyArr a <$> arrTypeExpr (NAnonymousBind : ctx)))
                       <|> return a)
                  <?> "type expression"

interTypeExpr :: [NameBinding] -> Parser Type
interTypeExpr ctx = do x <- arrTypeExpr ctx
                       xs <- many (reservedOp "&" >> arrTypeExpr ctx)
                       return $ case xs of
                         [] -> x
                         _ -> TyInter (x:xs)
                    <?> "type expression"

forallExpr :: [NameBinding] -> Parser Type
forallExpr ctx = do reserved "forall"
                    name <- identifier <?> "type variable"
                    bound <- option TyTop $ (reservedOp "<:" >> interTypeExpr ctx)
                    reservedOp "."
                    t <- typeExpr (NTyVarBind name : ctx) -- ???
                    return (TyAll name bound t)

typeExpr :: [NameBinding] -> Parser Type
typeExpr ctx = forallExpr ctx <|> interTypeExpr ctx
                 <?> "type expression"

simpleTerm :: [NameBinding] -> Parser Term
simpleTerm ctx = (reserved "True" >> return (TPrimValue $ PVBool True))
                 <|> (reserved "False" >> return (TPrimValue $ PVBool False))
                 <|> (try ((TPrimValue . PVReal) <$> float) <?> "floating point literal")
                 <|> (((TPrimValue . PVInt) <$> natural) <?> "integer literal")
                 <|> parens (term ctx)
                 <|> choice [reserved name >> return (TPrimValue v) | (name,v) <- builtinFunctions]
                 <|> (do name <- identifier <?> "variable"
                         case NVarBind name `elemIndex` ctx of
                           Just i -> return (TRef i name)
                           Nothing -> fail ("undefined variable: " ++ name))
                 <?> "simple expression"

-- function application / type application / type coercion
appTerm :: [NameBinding] -> Parser Term
appTerm ctx = do x <- simpleTerm ctx
                 xs <- many ((flip TApp <$> simpleTerm ctx)
                              <|> (flip TTyApp <$> brackets (typeExpr ctx))
                              <|> (flip TCoerce <$> (reserved "as" >> arrTypeExpr ctx)))
                 return (foldl' (flip ($)) x xs)

term :: [NameBinding] -> Parser Term
term ctx = lambdaAbstraction
           <|> typeAbstraction
           <|> ifThenElse
           <|> typeFor
           <|> appTerm ctx
           <?> "expression"
  where lambdaAbstraction = do reservedOp "\\"
                               name <- identifier
                               reservedOp ":"
                               varType <- typeExpr ctx
                               reservedOp "."
                               body <- term (NVarBind name : ctx)
                               return (TAbs name varType body)
        typeAbstraction = do reservedOp "?"
                             name <- identifier
                             bound <- option TyTop $ (reservedOp "<:" >> interTypeExpr ctx)
                             reservedOp "."
                             body <- term (NTyVarBind name : ctx)
                             return (TTyAbs name bound body)
        ifThenElse = do reserved "if"
                        cond <- term ctx
                        reserved "then"
                        thenExpr <- term ctx
                        reserved "else"
                        elseExpr <- term ctx
                        return (TIf cond thenExpr elseExpr)
        typeFor = do reserved "for"
                     name <- identifier
                     reserved "in"
                     candidates <- sepBy1 (interTypeExpr ctx) (reservedOp ",")
                     reservedOp "."
                     body <- term (NTyVarBind name : ctx)
                     return $ TFor name candidates body

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
