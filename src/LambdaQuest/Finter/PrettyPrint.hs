module LambdaQuest.Finter.PrettyPrint
  (prettyPrintTypeP
  ,prettyPrintType
  ,prettyPrintCanonicalTypeP
  ,prettyPrintICanonicalTypeP
  ,prettyPrintTermP
  ,prettyPrintTerm
  ) where
import LambdaQuest.Finter.Type
import LambdaQuest.Finter.Parse (NameBinding(..))
import Data.List (intersperse)

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

-- <3>: Int | Real | Bool | <type variable> | <0>
-- <2>: <3> -> <2>
-- <1>: <2> & .. & <2>
-- <0>: forall a <: <1>. <0> | <1>
prettyPrintTypeP :: Int -> [NameBinding] -> Type -> ShowS
prettyPrintTypeP p ctx t = case t of
  TyPrim PTyInt -> showString "Int"
  TyPrim PTyReal -> showString "Real"
  TyPrim PTyBool -> showString "Bool"
  TyPrim PTyUnit -> showString "Unit"
  TyTop -> showString "Top"
  TyArr s t -> showParen (p > 2) $ prettyPrintTypeP 3 ctx s . showString " -> " . prettyPrintTypeP 2 (NAnonymousBind : ctx) t
  TyRef i _ | i < length ctx -> case ctx !! i of
                                  NTyVarBind n -> showString n
                                  n -> showString "<invalid type variable reference #" . shows i . showString ": " . shows n . showChar '>'
            | otherwise -> showString "<invalid type variable reference #" . shows i . showString ", index out of range>"
  TyAll name TyTop t -> let name' = rename (tyVarNames ctx) name
                        in showParen (p > 0) $ showString "forall " . showString name' . showString ". " . prettyPrintTypeP 0 (NTyVarBind name' : ctx) t
  TyAll name b t -> let name' = rename (tyVarNames ctx) name
                    in showParen (p > 0) $ showString "forall " . showString name' . showString "<:" . prettyPrintTypeP 1 ctx b . showString ". " . prettyPrintTypeP 0 (NTyVarBind name' : ctx) t
  TyInter tys -> showParen (p > 1) $ foldr (.) id $ intersperse (showString " & ") $ map (prettyPrintTypeP 2 ctx) tys

prettyPrintType :: Type -> String
prettyPrintType t = prettyPrintTypeP 0 [] t ""

prettyPrintCanonicalTypeP :: Int -> [NameBinding] -> CCanonicalType -> ShowS
prettyPrintCanonicalTypeP p ctx [] = showString "Top"
prettyPrintCanonicalTypeP p ctx [t] = prettyPrintICanonicalTypeP p ctx t
prettyPrintCanonicalTypeP p ctx ts = showParen (p > 1) $ foldl1 (\x y -> x . showString " & " . y) $ map (prettyPrintICanonicalTypeP 2 ctx) ts

prettyPrintICanonicalTypeP :: Int -> [NameBinding] -> ICanonicalType -> ShowS
prettyPrintICanonicalTypeP p ctx t = case t of
  CTyPrim PTyInt -> showString "Int"
  CTyPrim PTyReal -> showString "Real"
  CTyPrim PTyBool -> showString "Bool"
  CTyPrim PTyUnit -> showString "Unit"
  CTyArr s t -> showParen (p > 2) $ prettyPrintCanonicalTypeP 3 ctx s . showString " -> " . prettyPrintICanonicalTypeP 2 (NAnonymousBind : ctx) t
  CTyRef i _ | i < length ctx -> case ctx !! i of
                 NTyVarBind n -> showString n
                 n -> showString "<invalid type variable reference #" . shows i . showString ": " . shows n . showChar '>'
             | otherwise -> showString "<invalid type variable reference #" . shows i . showString ", index out of range>"
  CTyAll name [] t -> showParen (p > 0) $ showString "forall " . showString name' . showString ". " . prettyPrintICanonicalTypeP 0 (NTyVarBind name' : ctx) t
    where name' = rename (tyVarNames ctx) name
  CTyAll name b t -> showParen (p > 0) $ showString "forall " . showString name' . showString "<:" . prettyPrintCanonicalTypeP 1 ctx b . showString ". " . prettyPrintICanonicalTypeP 0 (NTyVarBind name' : ctx) t
    where name' = rename (tyVarNames ctx) name

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
    BUnaryCommon f' -> case f' of
      BNegateInt -> "negateInt"
      BNegateReal -> "negateReal"
      BIntToReal -> "intToReal"
    BNegate -> "negate"
  TPrimValue (PVBuiltinBinary f) -> showString $ case f of
    BBinaryCommon f' -> case f' of
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
    BAdd -> "add"
    BSub -> "sub"
    BMul -> "mul"
  TAbs name ty body -> let name' = rename (varNames ctx) name
                       in showParen (p > 0) $ showChar '\\' . showString name' . showChar ':' . prettyPrintTypeP 1 ctx ty . showString ". " . prettyPrintTermP 0 (NVarBind name' : ctx) body
  TTyAbs name TyTop body -> let name' = rename (tyVarNames ctx) name
                            in showParen (p > 0) $ showString "?" . showString name' . showString ". " . prettyPrintTermP 0 (NTyVarBind name' : ctx) body
  TTyAbs name b body -> let name' = rename (tyVarNames ctx) name
                        in showParen (p > 0) $ showString "?" . showString name' . showString "<:" . prettyPrintTypeP 1 ctx b . showString ". " . prettyPrintTermP 0 (NTyVarBind name' : ctx) body
  TRef i _ | i < length ctx -> case ctx !! i of
                                 NVarBind n -> showString n
                                 n -> showString "<invalid variable reference #" . shows i . showString ": " . shows n . showChar '>'
           | otherwise -> showString "<invalid variable reference #" . shows i . showString ", index out of range>"
  TApp u v -> showParen (p > 1) $ prettyPrintTermP 1 ctx u . showChar ' ' . prettyPrintTermP 2 ctx v
  TTyApp u t -> showParen (p > 1) $ prettyPrintTermP 1 ctx u . showString " [" . prettyPrintTypeP 0 ctx t . showChar ']'
  TIf cond then_ else_ -> showParen (p > 0) $ showString "if " . prettyPrintTermP 0 ctx cond . showString " then " . prettyPrintTermP 0 ctx then_ . showString " else " . prettyPrintTermP 0 ctx else_
  TCoerce x t -> showParen (p > 1) $ prettyPrintTermP 1 ctx x . showString " as " . prettyPrintTypeP 1 ctx t
  TFor name candidates body -> let name' = rename (tyVarNames ctx) name
                               in showParen (p > 0) $ showString "for " . showString name' . showString " in " . foldr (.) id (intersperse (showString ", ") $ map (prettyPrintTypeP 1 ctx) candidates) . showString ". " . prettyPrintTermP 0 (NTyVarBind name' : ctx) body

prettyPrintTerm :: Term -> String
prettyPrintTerm t = prettyPrintTermP 0 [] t ""
