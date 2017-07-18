module LambdaQuest.SystemF.PrettyPrint
  (prettyPrintTypeP
  ,prettyPrintType
  ,prettyPrintTermP
  ,prettyPrintTerm
  ) where
import LambdaQuest.SystemF.Type

rename :: [String] -> String -> String
rename ctx name | name `notElem` ctx = name
                | otherwise = loop 1
  where loop :: Int -> String
        loop i = case name ++ show i of
          m | m `notElem` ctx -> m
          _ -> loop (i + 1)

-- <2>: Int | Real | Bool | <type variable> | <0>
-- <1>: <2> -> <1>
-- <0>: forall a. <0>
prettyPrintTypeP :: Int -> [String] -> Type -> ShowS
prettyPrintTypeP p ctx t = case t of
  TyPrim PTyInt -> showString "Int"
  TyPrim PTyReal -> showString "Real"
  TyPrim PTyBool -> showString "Bool"
  TyPrim PTyUnit -> showString "Unit"
  TyArr s t -> showParen (p > 1) $ prettyPrintTypeP 2 ctx s . showString " -> " . prettyPrintTypeP 1 ctx t
  TyRef i _ | i < length ctx -> showString (ctx !! i)
            | otherwise -> showString "<invalid reference #" . shows i . showChar '>'
  TyAll name t -> let name' = rename ctx name
                  in showParen (p > 0) $ showString "forall " . showString name' . showString ". " . prettyPrintTypeP 0 (name' : ctx) t

prettyPrintType :: Type -> String
prettyPrintType t = prettyPrintTypeP 0 [] t ""

-- <2>: <primitive> | <variable> | (<0>)
-- <1>: <1> <2> | <1> [ty]
-- <0>: \x:t. <0> | ?a. <0> | if <0> then <0> else <0>
prettyPrintTermP :: Int -> [String] -> [String] -> Term -> ShowS
prettyPrintTermP p tyctx ctx t = case t of
  TPrimValue (PVInt x) -> shows x
  TPrimValue (PVReal x) -> shows x
  TPrimValue (PVBool x) -> shows x
  TPrimValue PVUnit -> showString "unit"
  TPrimValue (PVBuiltinUnary f) -> showString $ case f of
    BNegateInt -> "negateInt"
    BNegateReal -> "negateReal"
    BIntToReal -> "intToReal"
  TPrimValue (PVBuiltinBinary f) -> showString $ case f of
    BAddInt -> "addInt"
    BSubInt -> "subInt"
    BMulInt -> "mulInt"
    BLtInt -> "ltInt"
    BLeInt -> "leInt"
    BEqualInt -> "equalInt"
    BAddReal -> "addReal"
    BSubReal -> "subReal"
    BMulReal -> "mulReal"
    BDivReal -> "divReal"
    BLtReal -> "ltReal"
    BLeReal -> "leReal"
    BEqualReal -> "equalReal"
  TAbs name ty body -> let name' = rename ctx name
                       in showParen (p > 0) $ showChar '\\' . showString name' . showChar ':' . prettyPrintTypeP 1 tyctx ty . showString ". " . prettyPrintTermP 0 tyctx (name' : ctx) body
  TTyAbs name body -> let name' = rename ctx name
                      in showParen (p > 0) $ showString "?" . showString name' . showString ". " . prettyPrintTermP 0 (name' : tyctx) ctx body
  TRef i _ | i < length ctx -> showString (ctx !! i)
           | otherwise -> showString "<invalid reference #" . shows i . showChar '>'
  TApp u v -> showParen (p > 1) $ prettyPrintTermP 1 tyctx ctx u . showChar ' ' . prettyPrintTermP 2 tyctx ctx v
  TTyApp u t -> showParen (p > 1) $ prettyPrintTermP 1 tyctx ctx u . showString " [" . prettyPrintTypeP 0 tyctx t . showChar ']'
  TIf cond then_ else_ -> showParen (p > 0) $ showString "if " . prettyPrintTermP 0 tyctx ctx cond . showString " then " . prettyPrintTermP 0 tyctx ctx then_ . showString " else " . prettyPrintTermP 0 tyctx ctx else_

prettyPrintTerm :: Term -> String
prettyPrintTerm t = prettyPrintTermP 0 [] [] t ""
