module Intriky.Lexer where

import Control.Monad   (liftM, mzero)
import Data.ByteString (pack)
import Data.Char       (chr, ord, isDigit, toLower)
import Data.Ratio      ((%), numerator, denominator)
import Data.Word       (Word8)
import Text.ParserCombinators.Parsec hiding (digit, letter, hexDigit, label)

import Intriky.Types

data IntrikyToken
    = IdentTok String
    | BoolTok Bool
    | NumberTok IntrikyNumber
    | CharTok Char
    | StringTok String
    | LParenTok
    | RParenTok
    | VectorTok
    | BytevectorTok
    | QuoteTok
    | BacktickTok
    | CommaTok
    | SeqCommaTok
    | DotTok

token' :: Parser IntrikyToken
token' = choice
    [ identifier
    , boolean
    , liftM NumberTok number
    , character
    , string'
    , (char '('      >> return LParenTok)
    , (char ')'      >> return RParenTok)
    , (string "#("   >> return VectorTok)
    , (string "#u8(" >> return BytevectorTok)
    , (char '\''     >> return QuoteTok)
    , (char '`'      >> return BacktickTok)
    , (char ','      >> return CommaTok)
    , (string ",@"   >> return SeqCommaTok)
    , (char '.'      >> return DotTok)
    ]

delimiter :: Parser Char
delimiter =
      whitespace
  <|> verticalLine
  <|> char '('
  <|> char ')'
  <|> char '\"'
  <|> char ';'

intralineWhitespace :: Parser Char
intralineWhitespace = oneOf " \t"

whitespace :: Parser Char
whitespace = intralineWhitespace <|> lineEnding

verticalLine :: Parser Char
verticalLine = char '|'

lineEnding :: Parser Char
lineEnding = newline >> string "\r\n" >> char '\r'

comment :: Parser ()
comment = choice
  [ char ';' >> manyTill anyChar lineEnding >> return ()
  , nestedComment
--  , string "#;" >> intertokenSpace >> datum
  ] >> return ()

nestedComment :: Parser ()
nestedComment =
      string "#|"
   >> commentText
   >> many commentCont
   >> string "|#"
   >> return ()

commentText :: Parser ()
commentText = manyTill anyChar (string "#|" <|> string "|#") >> return ()

commentCont :: Parser ()
commentCont = nestedComment >> commentText

-- TODO: currently these are ignored.
directive :: Parser ()
directive = (string "#!fold-case" <|> string "#!no-fold-case") >> return ()

atmosphere :: Parser ()
atmosphere =
      (whitespace >> return ())
  <|> comment
  <|> (directive >> intertokenSpace)
  <|> (many atmosphere >> return ())

intertokenSpace :: Parser ()
intertokenSpace = many atmosphere >> return ()

identifier :: Parser IntrikyToken
identifier = choice [regular, vert, peculiarIdentifier]
  where
    regular = do
        x <- initial
        xs <- many subsequent
        return $ IdentTok (x:xs)
    vert = do
        verticalLine
        xs <- many symbolElement
        verticalLine
        return (IdentTok xs)

initial :: Parser Char
initial = letter <|> specialInitial

letter :: Parser Char
letter = oneOf $ ['a'..'z'] ++ ['A'..'Z']

specialInitial :: Parser Char
specialInitial = oneOf "~$%&*/:<=>?^_~"

subsequent :: Parser Char
subsequent = choice [initial, digit, specialSubsequent]

digit :: Parser Char
digit = oneOf ['0'..'9']

hexDigit :: Parser Char
hexDigit = digit <|> oneOf (['a'..'f'] ++ ['A'..'F'])

explicitSign :: Parser Char
explicitSign = oneOf "+-"

specialSubsequent :: Parser Char
specialSubsequent = explicitSign <|> oneOf ".@"

inlineHexEscape :: Parser Char
inlineHexEscape = do
    string "\\x"
    x <- hexScalarValue
    string ";"
    return (chr x)

hexScalarValue :: Parser Int
hexScalarValue = many1 hexDigit >>= return . read

mnemonicEscape :: Parser Char
mnemonicEscape = choice
    [ string "\\a" >> return '\a'
    , string "\\b" >> return '\b'
    , string "\\t" >> return '\t'
    , string "\\n" >> return '\n'
    , string "\\r" >> return '\r'
    ]
 
peculiarIdentifier :: Parser IntrikyToken
peculiarIdentifier = fmap IdentTok $ choice [plusMinus, noDot, withDot]
  where
    plusMinus = explicitSign >>= return . return
    noDot = do
        x <- explicitSign
        y <- signSubsequent
        zs <- many subsequent
        return (x:y:zs)
    withDot = do
        x <- optionMaybe explicitSign
        char '.'
        y <- dotSubsequent
        zs <- many subsequent
        let result = '.':y:zs
        return $ maybe result (:result) x

dotSubsequent :: Parser Char
dotSubsequent = choice [signSubsequent, char '.']

signSubsequent :: Parser Char
signSubsequent = choice [initial, explicitSign, char '@']

symbolElement :: Parser Char
symbolElement = choice
    [ noneOf "|\\"
    , inlineHexEscape
    , mnemonicEscape
    , string "\\|" >> char '|'
    ]

boolean :: Parser IntrikyToken
boolean = fmap BoolTok $ choice [t, f]
  where
    t = choice [string "#t", string "#true"]  >> return True
    f = choice [string "#f", string "#false"] >> return False

character :: Parser IntrikyToken
character = choice
    [ string "#\\" >> anyChar >>= return . CharTok
    , string "#\\" >> characterName >>= return . CharTok
    , string "#\\x" >> hexScalarValue >>= return . CharTok . chr
    ]

characterName :: Parser Char
characterName = choice
    [ string "alarm"     >> return '\a'
    , string "backspace" >> return '\b'
    , string "delete"    >> return '\x7f'
    , string "escape"    >> return '\x1b'
    , string "newline"   >> return '\n'
    , string "null"      >> return '\0'
    , string "return"    >> return '\r'
    , string "space"     >> return ' '
    , string "tab"       >> return '\t'
    ]

string' :: Parser IntrikyToken
string' = between q q (many stringElement) >>= return . StringTok
  where q = char '"'
 
stringElement :: Parser Char
stringElement = choice
    [ noneOf "\"\\"
    , mnemonicEscape
    , string "\\\"" >> return '"'
    , string "\\\\" >> return '\\'
    , char '\\' >> many intralineWhitespace >> lineEnding
                >> many intralineWhitespace >> stringElement
    , inlineHexEscape
    ]

-- Parses a number.
number :: Parser IntrikyNumber
number = choice [num 2, num 8, num 10, num 16]

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
        digits <- many1 (digit' 10)
        let x = readInteger 10 digits
            y = 10 ^ (length digits)
        return (x % y)
    both = do
        before <- many1 (digit' 10)
        char '.'
        after <- many (digit' 10)
        let x = readInteger 10 (before ++ after)
            y = 10 ^ (length after)
        return (x % y)
decimal _ = mzero

-- Parses an unsigned decimal integer.
uinteger :: Int -> Parser Integer
uinteger r = liftM (readInteger r) $ many1 (digit' r)
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

-- Simple data type to help parse numbers.
data Exactness = E | I

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
    digits <- many1 (digit' 10)
    return $ s (readInteger 10 digits)

-- Parses an exponent marker.
exponentMarker :: Parser ()
exponentMarker = oneOf "eE" >> return ()

-- Functions to represent the sign of a number.
plus, minus :: Num a => a -> a
plus = id
minus = negate

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
digit' :: Int -> Parser Char
digit' 2 = oneOf "01"
digit' 8 = oneOf ['0'..'7']
digit' 10 = digit
digit' 16 = hexDigit
