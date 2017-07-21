module LambdaQuest.SystemFsub.PrettyPrint
  (prettyPrintTypeP
  ,prettyPrintType
  ,prettyPrintTermP
  ,prettyPrintTerm
  ) where
import LambdaQuest.SystemFsub.Type
import LambdaQuest.SystemFsub.Parse (NameBinding(..))

varNames :: [NameBinding] -> [String]
varNames = foldr (\x ys -> case x of
                             NVarBind n -> n:ys
                             _ -> ys) []

tyVarNames :: [NameBinding] -> [String]
tyVarNames = foldr (\x ys -> case x of
                               NTyVarBind n -> n:ys
                               _ -> ys) []

rename :: [String] -> String -> String
rename ctx name | name `notElem` ctx = name
                | otherwise = loop 1
  where loop :: Int -> String
        loop i = case name ++ show i of
          m | m `notElem` ctx -> m
          _ -> loop (i + 1)

-- <2>: Int | Real | Bool | <type variable> | <0>
-- <1>: <2> -> <1>
-- <0>: forall a <: <1>. <0>
prettyPrintTypeP :: Int -> [NameBinding] -> Type -> ShowS
prettyPrintTypeP p ctx t = case t of
  TyPrim PTyInt -> showString "Int"
  TyPrim PTyReal -> showString "Real"
  TyPrim PTyBool -> showString "Bool"
  TyPrim PTyUnit -> showString "Unit"
  TyTop -> showString "Top"
  TyArr s t -> showParen (p > 1) $ prettyPrintTypeP 2 ctx s . showString " -> " . prettyPrintTypeP 1 (NAnonymousBind : ctx) t
  TyRef i _ | i < length ctx -> case ctx !! i of
                                  NTyVarBind n -> showString n
                                  n -> showString "<invalid type variable reference #" . shows i . showString ": " . shows n . showChar '>'
            | otherwise -> showString "<invalid type variable reference #" . shows i . showString ", index out of range>"
  TyAll name TyTop t -> let name' = rename (tyVarNames ctx) name
                        in showParen (p > 0) $ showString "forall " . showString name' . showString ". " . prettyPrintTypeP 0 (NTyVarBind name' : ctx) t
  TyAll name b t -> let name' = rename (tyVarNames ctx) name
                    in showParen (p > 0) $ showString "forall " . showString name' . showString "<:" . prettyPrintTypeP 1 ctx b . showString ". " . prettyPrintTypeP 0 (NTyVarBind name' : ctx) t

prettyPrintType :: Type -> String
prettyPrintType t = prettyPrintTypeP 0 [] t ""

-- <2>: <primitive> | <variable> | (<0>)
-- <1>: <1> <2> | <1> [ty] | <1> as <type>
-- <0>: \x:t. <0> | ?a. <0> | if <0> then <0> else <0>
prettyPrintTermP :: Int -> [NameBinding] -> Term -> ShowS
prettyPrintTermP p ctx t = case t of
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
  TAbs name ty body -> let name' = rename (varNames ctx) name
                       in showParen (p > 0) $ showChar '\\' . showString name' . showChar ':' . prettyPrintTypeP 1 ctx ty . showString ". " . prettyPrintTermP 0 (NVarBind name' : ctx) body
  TTyAbs name TyTop body -> let name' = rename (varNames ctx) name
                            in showParen (p > 0) $ showString "?" . showString name' . showString ". " . prettyPrintTermP 0 (NTyVarBind name' : ctx) body
  TTyAbs name b body -> let name' = rename (varNames ctx) name
                        in showParen (p > 0) $ showString "?" . showString name' . showString "<:" . prettyPrintTypeP 1 ctx b . showString ". " . prettyPrintTermP 0 (NTyVarBind name' : ctx) body
  TRef i _ | i < length ctx -> case ctx !! i of
                                 NVarBind n -> showString n
                                 n -> showString "<invalid variable reference #" . shows i . showString ": " . shows n . showChar '>'
           | otherwise -> showString "<invalid variable reference #" . shows i . showString ", index out of range>"
  TApp u v -> showParen (p > 1) $ prettyPrintTermP 1 ctx u . showChar ' ' . prettyPrintTermP 2 ctx v
  TTyApp u t -> showParen (p > 1) $ prettyPrintTermP 1 ctx u . showString " [" . prettyPrintTypeP 0 ctx t . showChar ']'
  TIf cond then_ else_ -> showParen (p > 0) $ showString "if " . prettyPrintTermP 0 ctx cond . showString " then " . prettyPrintTermP 0 ctx then_ . showString " else " . prettyPrintTermP 0 ctx else_
  TCoerce x t -> showParen (p > 1) $ prettyPrintTermP 1 ctx x . showString " as " . prettyPrintTypeP 1 ctx t

prettyPrintTerm :: Term -> String
prettyPrintTerm t = prettyPrintTermP 0 [] t ""
