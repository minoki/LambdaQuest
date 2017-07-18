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
