{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LambdaQuest.Common.Parse
  (naturalOrFloat
  ,natFloat
  ,makeDecimal
  ,makeHexadecimal
  ) where
import Text.Parsec
import qualified Text.Parsec.Token as PT
import Data.Foldable (foldl')
import Data.Char (digitToInt)
import Data.Ratio

{-| This is an alternative of Parsec's 'PT.naturalOrFloat' with the support for hexadecimal floats.

The return type for floats are generalized to 'Fractional', to allow 'Rational' to be returned directly.

The syntax for numbers here differs from C11's in the following ways:

  - Octal integers needs @0o@ or @0O@ as prefix, instead of @0@.
  - In hexadecimal float literal, binary-exponent-part can be omitted (because we have no @f@ as floating-suffix).
  - decimal-floating-constant without digits before the decimal mark (@.@) are not allowed.

>>> let parseNaturalOrFloat = parse (naturalOrFloat Text.Parsec.Language.haskell) ""
>>> parseNaturalOrFloat "0x123"
Right (Left 291)
>>> parseNaturalOrFloat "0x123p0"
Right (Right 291.0)
>>> parseNaturalOrFloat "0x123.45"
Right (Right 291.26953125)
>>> parseNaturalOrFloat "0x123.45p8"
Right (Right 74565.0)
>>> parseNaturalOrFloat "0x.1"
Right (Right 6.25e-2)
>>> parseNaturalOrFloat "123.45e2"
Right (Right 12345.0)
>>> parseNaturalOrFloat "0o123"
Right (Left 83)
-}
naturalOrFloat :: (Stream s m Char, Fractional r) => PT.GenTokenParser s u m -> ParsecT s u m (Either Integer r)
naturalOrFloat tokenParser = PT.lexeme tokenParser natFloat

{-| This is a parser for natural numbers, decimal floats, and hexadecimal floats.

Since this parser doesn't skip trailing whitespaces, it may be used to build a parser for imaginary numbers.

>  -+- '0' -+- ('x' | 'X') -+- <hex digits> -+- '.' <hex digits>opt <binary exponent part>opt  ==> hexadecimal float
>   |       |               |                +- <binary exponent part>                         ==> hexadecimal float
>   |       |               |                \---                                              ==> hexadecimal integer
>   |       |               \- '.' <hex digits> <binary exponent part>opt                      ==> hexadecimal float
>   |       +- ('o' | 'O') <octal digits>                       ==> octal integer
>   |       +- <digits> -+- '.' <digits>opt <exponent part>opt  ==> decimal float
>   |       |            +- <exponent part>                     ==> decimal float
>   |       |            \---                                   ==> decimal integer
>   |       +- '.' <digits>opt <exponent part>opt               ==> decimal float
>   |       +- <exponent part>                                  ==> decimal float (0.0)
>   |       \---                                                ==> decimal integer (0)
>   \- digits -+- '.' <digits>opt <exponent part>opt  ==> decimal float
>              +- exponent-part                       ==> decimal float
>              \---                                   ==> decimal integer
-}
natFloat :: forall s m r. (Stream s m Char, Fractional r) => forall u. ParsecT s u m (Either Integer r)
natFloat = do{ char '0'
             ; (oneOf "xX" >> hexadecimal)
               <|> (oneOf "oO" >> (Left <$> octalInt))
               <|> {- <digits> -} decimalFloat
               <|> {- '.' or <exponent part> or nothing -} decimalFractFloatOrInt 0
             }
           <|> {- <digits> -} decimalFloat
  where
    {-
      -+- <hex digits> -+- '.' <hex digits>opt <binary exponent part>opt  ==> hexadecimal float
       |                +- <binary exponent part>                         ==> hexadecimal float
       |                \--                                               ==> hexadecimal integer
       \- '.' <hex digits> <binary exponent part>opt                      ==> hexadecimal float
    -}
    hexadecimal :: ParsecT s u m (Either Integer r)
    hexadecimal = do{ n <- digits 16 hexDigit
                    ; (Right <$> hexFractExponent n) <|> pure (Left n)
                    }
              <|> do{ char '.'
                    ; Right <$> (makeHexadecimal 0 <$> many1 hexDigit <*> binaryExponentOpt)
                    }

    {-
      -+- '.' <hex digits>opt <binary exponent part>opt
       \- <binary exponent part>
    -}
    hexFractExponent :: Integer -> ParsecT s u m r
    hexFractExponent n = do{ char '.'
                           ; makeHexadecimal n <$> many hexDigit <*> binaryExponentOpt
                           }
                     <|> (makeHexadecimal n [] <$> binaryExponent)

    {- <octal digits> -}
    octalInt :: ParsecT s u m Integer
    octalInt = digits 8 octDigit

    {-
      <digits> -+- '.' <digits>opt <exponent part>opt  ==> decimal float
                +- <exponent part>                     ==> decimal float
                \-                                     ==> decimal integer
    -}
    decimalFloat :: ParsecT s u m (Either Integer r)
    decimalFloat = do{ n <- digits 10 digit
                     ; decimalFractFloatOrInt n
                     }

    {-
      -+- '.' <digits>opt <exponent part>opt  ==> decimal float
       +- <exponent part>                     ==> decimal float
       \-                                     ==> decimal integer
    -}
    decimalFractFloatOrInt :: Integer -> ParsecT s u m (Either Integer r)
    decimalFractFloatOrInt n = (Right <$> decimalFractExponent n) <|> pure (Left n)

    {-
      -+- '.' <digits>opt <exponent part>opt
       \- <exponent part>
    -}
    decimalFractExponent :: Integer -> ParsecT s u m r
    decimalFractExponent n = do{ char '.'
                               ; makeDecimal n <$> many digit <*> option 0 decimalExponent
                               }
                         <|> (makeDecimal n [] <$> decimalExponent)

    decimalExponent, binaryExponent, binaryExponentOpt :: ParsecT s u m Int
    decimalExponent = oneOf "eE" >> signedInt <?> "exponent"
    binaryExponent = oneOf "pP" >> signedInt <?> "binary exponent"
    binaryExponentOpt = option 0 binaryExponent

    digits :: Integer -> ParsecT s u m Char -> ParsecT s u m Integer
    digits base baseDigit = foldl' (\x d -> x * base + toInteger (digitToInt d)) 0 <$> many1 baseDigit

    signedInt :: ParsecT s u m Int
    signedInt = do{ sign <- (char '-' >> pure (-1)) <|> (optional (char '+') >> pure 1)
                  ; d <- fromInteger <$> digits 10 digit
                  ; pure $! sign * d
                  }

{-# SPECIALIZE naturalOrFloat :: (Stream s m Char) => PT.GenTokenParser s u m -> ParsecT s u m (Either Integer Double) #-}
{-# SPECIALIZE naturalOrFloat :: (Stream s m Char) => PT.GenTokenParser s u m -> ParsecT s u m (Either Integer Rational) #-}
{-# SPECIALIZE natFloat :: (Stream s m Char) => ParsecT s u m (Either Integer Double) #-}
{-# SPECIALIZE natFloat :: (Stream s m Char) => ParsecT s u m (Either Integer Rational) #-}

-- | Make a decimal float from integer part, fractional part, and exponential part.
--
-- >>> makeDecimal 123 "45" 6 == 123.45e+6
-- True
makeDecimal :: Fractional r => Integer -> [Char] -> Int -> r
makeDecimal = makeFractional 10 1

-- | Make a hexadecimal float from integer part, fractional part, and binary exponential part.
--
-- >>> makeHexadecimal 0xdead "beef" (-4) == 0xdeadbeef * 2^^(-20) {- 0xdead.beefp-4 -}
-- True
makeHexadecimal :: Fractional r => Integer -> [Char] -> Int -> r
makeHexadecimal = makeFractional 2 4

makeFractional :: Fractional r => Integer -> Int -> Integer -> [Char] -> Int -> r
makeFractional base p n d e | e' < 0    = fromInteger (x * base ^ (-e'))
                            | otherwise = fromRational (x % (base ^ e'))
  where
    e' = p * length d - e
    x = foldl' (\x d -> x * (base ^ p) + toInteger (digitToInt d)) n d
