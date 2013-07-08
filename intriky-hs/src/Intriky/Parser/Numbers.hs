module Intriky.Parser.Numbers (parseNumber) where

import Control.Monad (liftM, mzero)
import Data.Char     (ord, isDigit, toLower)
import Data.Ratio    (numerator, denominator, (%))
import Text.ParserCombinators.Parsec hiding (digit)

import Intriky.Types (IntrikyNumber(..), NumPart(..), SpecialNum(..))

plus, minus :: Num a => a -> a
plus = id
minus = negate

data Exactness = E | I

-- Parses a number.
parseNumber :: Parser IntrikyNumber
parseNumber = choice [num 2, num 8, num 10, num 16]

-- Parses a number with a given radix.
num :: Int -> Parser IntrikyNumber
num r = do
    p <- prefix r
    (real, imag) <- complex r
    case p of
        E -> return $ Number real imag
        I -> return $ Number (toInexact real) (toInexact imag)
  where
    toInexact (Exact n) = Inexact (fromIntegral x / fromIntegral y)
      where x = numerator n
            y = denominator n
    toInexact x = x

-- Parses a complex number with a given radix.
complex :: Int -> Parser (NumPart, NumPart)
complex r = choice [allReal, polar, special, others]
  where
    unit = oneOf "iI"
    allReal = do
        x <- real r
        return (x, Exact 0)
    polar = do
        x <- real r
        char '@'
        y <- real r
        return undefined -- TODO calculate polar stuff instead
    special = do
        x <- option (Exact 0) (real r)
        y <- infnan
        unit
        return (x, y)
    others = do
        x <- option (Exact 0) (real r)
        s <- sign
        y <- option 0 (ureal r)
        unit
        return (x, Exact (s y))

-- Parses a signed real or a special number.
real :: Int -> Parser NumPart
real r = choice
    [ do s <- sign; x <- ureal r; return $ Exact (s x)
    , infnan
    ]

-- Parses an unsigned real number.
ureal :: Int -> Parser Rational
ureal r = choice
    [ liftM toRational (uinteger r)
    , ratioLiteral
    , decimal r
    ]
  where
    ratioLiteral = do
        x <- uinteger r
        char '/'
        y <- uinteger r
        return (x % y)

-- Parses an unsigned decimal number.
decimal :: Int -> Parser Rational
decimal 10 = do
    x <- choice [whole, fractional, both]
    s <- suffix
    return $ maybe x (\y -> x * fromInteger (10 ^ y)) s
  where
    whole = liftM toRational (uinteger 10)
    fractional = do
        char '.'
        digits <- many1 (digit 10)
        let x = readInteger 10 digits
            y = 10 ^ (length digits)
        return (x % y)
    both = do
        before <- many1 (digit 10)
        char '.'
        after <- many (digit 10)
        let x = readInteger 10 (before ++ after)
            y = 10 ^ (length after)
        return (x % y)
decimal _ = mzero

-- Parses an unsigned decimal integer.
uinteger :: Int -> Parser Integer
uinteger r = liftM (readInteger r) $ many1 (digit r)
  where

-- Parses an integer from a string given a radix.
readInteger :: Num a => Int -> String -> a
readInteger _ [] = 0
readInteger r (x:xs) = x' + r' * readInteger r xs
  where
    r' = fromIntegral r
    x' = fromIntegral $ if isDigit x
                            then ord x - ord '0'
                            else ord (toLower x) - ord 'a'

-- Parses radix and exactness prefixes.
prefix :: Int -> Parser Exactness
prefix r = choice
    [ radix r >> exactness
    , do  x <- exactness; radix r; return x
    ]

-- Parses an infinity or NaN.
infnan :: Parser NumPart
infnan = liftM Special $ choice
    [ string "+inf.0" >> return PlusInf
    , string "-inf.0" >> return MinusInf
    , string "+nan.0" >> return PlusNan
    , string "-nan.0" >> return MinusNan
    ]

-- Parses an exponent suffix.
suffix :: Parser (Maybe Integer)
suffix = optionMaybe $ do
    exponentMarker
    s <- sign
    digits <- many1 (digit 10)
    return $ s (readInteger 10 digits)

-- Parses an exponent marker.
exponentMarker :: Parser ()
exponentMarker = oneOf "eE" >> return ()

-- Parses a sign, and returns a function that applies the sign.
sign :: Num a => Parser (a -> a)
sign = option plus $ choice
    [ char '+' >> return plus
    , char '-' >> return minus
    ]

-- Parses an exactness prefix.
exactness :: Parser Exactness
exactness = option E $ choice [i, e]
  where
    i = char '#' >> oneOf "iI" >> return I
    e = char '#' >> oneOf "eE" >> return E

-- Parses a radix prefix.
radix :: Int -> Parser ()
radix 2 = string "#b" >> return ()
radix 8 = string "#o" >> return ()
radix 10 = optional (string "#d")
radix 16 = string "#d" >> return ()

-- Parses a digit for a number with a given radix.
digit :: Int -> Parser Char
digit 2 = oneOf "01"
digit 8 = oneOf ['0'..'7']
digit 10 = oneOf ['0'..'9']
digit 16 = digit 10 <|> oneOf ['a'..'f'] <|> oneOf ['A'..'F']
