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

data PrimValue = PVInt !Integer
               | PVReal !Double
               | PVBool !Bool
               | PVUnit
               | PVBuiltinUnary !BuiltinUnaryFn
               | PVBuiltinBinary !BuiltinBinaryFn
               deriving (Eq,Show)

genPrimTypeOf :: (PrimType -> t) -> (t -> t -> t) -> (PrimValue -> t)
genPrimTypeOf tyPrim tyArr = primTypeOf
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
      PVBuiltinUnary f -> builtinUnaryFnType f
      PVBuiltinBinary f -> builtinBinaryFnType f
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
