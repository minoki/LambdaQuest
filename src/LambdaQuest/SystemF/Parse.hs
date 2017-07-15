module LambdaQuest.SystemF.Parse where
import LambdaQuest.SystemF.Type
import Text.Parsec
import qualified Text.Parsec.Token as PT
import Data.List (elemIndex)
import Data.Foldable (foldl')

type Parser = Parsec String ()

langDef = PT.LanguageDef { PT.commentStart = ""
                         , PT.commentEnd = ""
                         , PT.commentLine = ""
                         , PT.nestedComments = False
                         , PT.identStart = letter <|> char '_'
                         , PT.identLetter = alphaNum <|> char '_'
                         , PT.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                         , PT.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                         , PT.reservedNames = ["if","then","else","True","False","forall","as","Int","Real","Bool","let","in"]
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

simpleTypeExpr :: [String] -> Parser Type
simpleTypeExpr tyctx = (reserved "Int" >> return TyInt)
                       <|> (reserved "Real" >> return TyReal)
                       <|> (reserved "Bool" >> return TyBool)
                       <|> parens (typeExpr tyctx)
                       <|> (do name <- identifier <?> "type variable"
                               case name `elemIndex` tyctx of
                                 Just i -> return (TyRef i)
                                 Nothing -> fail ("undefined type variable: " ++ name))
                       <?> "simple type expression"

arrTypeExpr :: [String] -> Parser Type
arrTypeExpr tyctx = do a <- simpleTypeExpr tyctx
                       (reservedOp "->" >> (TyArr a <$> arrTypeExpr tyctx))
                         <|> return a
                    <?> "type expression"

typeExpr :: [String] -> Parser Type
typeExpr tyctx = forallExpr <|> arrTypeExpr tyctx
                 <?> "type expression"
  where forallExpr = do reserved "forall"
                        name <- identifier <?> "type variable"
                        reservedOp "."
                        t <- typeExpr (name : tyctx)
                        return (TyAll name t)

simpleTerm :: [String] -> [String] -> Parser Term
simpleTerm tyctx ctx = (reserved "True" >> return (TValue $ VBool True))
                       <|> (reserved "False" >> return (TValue $ VBool False))
                       <|> (try ((TValue . VReal) <$> float) <?> "floating point literal")
                       <|> (((TValue . VInt) <$> natural) <?> "integer literal")
                       <|> parens (term tyctx ctx)
                       <|> (do name <- identifier <?> "variable"
                               case name `elemIndex` ctx of
                                 Just i -> return (TRef i)
                                 Nothing -> fail ("undefined variable: " ++ name))
                       <?> "simple expression"

-- function application / type application
appTerm :: [String] -> [String] -> Parser Term
appTerm tyctx ctx = do x <- simpleTerm tyctx ctx
                       xs <- many ((Left <$> simpleTerm tyctx ctx) <|> (Right <$> brackets (typeExpr tyctx)))
                       return (foldl' (\x -> either (TApp x) (TTyApp x)) x xs)

term :: [String] -> [String] -> Parser Term
term tyctx ctx = lambdaAbstraction
                 <|> typeAbstraction
                 <|> ifThenElse
                 <|> appTerm tyctx ctx
                 <?> "expression"
  where lambdaAbstraction = do reservedOp "\\"
                               name <- identifier
                               reservedOp ":"
                               varType <- typeExpr tyctx
                               reservedOp "."
                               body <- term tyctx (name : ctx)
                               return (TAbs name varType body)
        typeAbstraction = do reservedOp "?"
                             name <- identifier
                             reservedOp "."
                             body <- term (name : tyctx) ctx
                             return (TTyAbs name body)
        ifThenElse = do reserved "if"
                        cond <- term tyctx ctx
                        reserved "then"
                        thenExpr <- term tyctx ctx
                        reserved "else"
                        elseExpr <- term tyctx ctx
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
          t <- term [] []
          eof
          return t
