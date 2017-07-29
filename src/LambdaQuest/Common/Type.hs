module LambdaQuest.Common.Type where

data PrimType = PTyInt
              | PTyReal
              | PTyBool
              | PTyUnit
              deriving (Eq,Show,Enum,Bounded)

data BuiltinUnaryFn = BNegateInt
                    | BNegateReal
                    | BIntToReal
                    deriving (Eq,Show,Enum,Bounded)

data BuiltinBinaryFn = BAddInt
                     | BSubInt
                     | BMulInt
                     | BLtInt
                     | BLeInt
                     | BEqualInt
                     | BAddReal
                     | BSubReal
                     | BMulReal
                     | BDivReal
                     | BLtReal
                     | BLeReal
                     | BEqualReal
                     deriving (Eq,Show,Enum,Bounded)

data PrimValueT unary binary = PVInt !Integer
                             | PVReal !Double
                             | PVBool !Bool
                             | PVUnit
                             | PVBuiltinUnary !unary
                             | PVBuiltinBinary !binary
                             deriving (Eq,Show)
type PrimValue = PrimValueT BuiltinUnaryFn BuiltinBinaryFn

genPrimTypeOf :: (PrimType -> t) -> (t -> t -> t) -> (PrimValue -> t)
genPrimTypeOf = genPrimTypeOfT ($) ($)

genPrimTypeOfT :: ((BuiltinUnaryFn -> t) -> unary -> t) -> ((BuiltinBinaryFn -> t) -> binary -> t) -> (PrimType -> t) -> (t -> t -> t) -> (PrimValueT unary binary -> t)
genPrimTypeOfT unaryFn binaryFn tyPrim tyArr = primTypeOf
  where
    tyInt = tyPrim PTyInt
    tyReal = tyPrim PTyReal
    tyBool = tyPrim PTyBool
    tyUnit = tyPrim PTyUnit
    primTypeOf p = case p of
      PVInt _ -> tyInt
      PVReal _ -> tyReal
      PVBool _ -> tyBool
      PVUnit -> tyUnit
      PVBuiltinUnary f -> unaryFn builtinUnaryFnType f
      PVBuiltinBinary f -> binaryFn builtinBinaryFnType f
    builtinUnaryFnType f = case f of
      BNegateInt -> tyArr tyInt tyInt
      BNegateReal -> tyArr tyReal tyReal
      BIntToReal -> tyArr tyInt tyReal
    builtinBinaryFnType f = case f of
      BAddInt -> tyArr tyInt (tyArr tyInt tyInt)
      BSubInt -> tyArr tyInt (tyArr tyInt tyInt)
      BMulInt -> tyArr tyInt (tyArr tyInt tyInt)
      BLtInt -> tyArr tyInt (tyArr tyInt tyBool)
      BLeInt -> tyArr tyInt (tyArr tyInt tyBool)
      BEqualInt -> tyArr tyInt (tyArr tyInt tyBool)
      BAddReal -> tyArr tyReal (tyArr tyReal tyReal)
      BSubReal -> tyArr tyReal (tyArr tyReal tyReal)
      BMulReal -> tyArr tyReal (tyArr tyReal tyReal)
      BDivReal -> tyArr tyReal (tyArr tyReal tyReal)
      BLtReal -> tyArr tyReal (tyArr tyReal tyBool)
      BLeReal -> tyArr tyReal (tyArr tyReal tyBool)
      BEqualReal -> tyArr tyReal (tyArr tyReal tyBool)
