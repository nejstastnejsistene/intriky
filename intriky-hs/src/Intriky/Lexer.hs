module Intriky.Lexer where

import Control.Monad   (liftM)
import Data.Char       (chr, ord, isDigit, toLower)
import Data.Ratio      ((%), numerator, denominator)
import Text.ParserCombinators.Parsec hiding
                       (digit, letter, hexDigit, label, token)

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

instance Show IntrikyToken where
    show (IdentTok x) = x
    show (BoolTok x) = if x then "#t" else "#f"
    show (NumberTok _) = "<Number>"
    show (CharTok x) = "'" ++ [x] ++ "'"
    show (StringTok x) = show x
    show LParenTok = "("
    show RParenTok = ")"
    show VectorTok = "#("
    show BytevectorTok = "#u8("
    show QuoteTok = "'"
    show BacktickTok = "`"
    show CommaTok = ","
    show SeqCommaTok = ",@"
    show DotTok = "."

-- Simple data type to help parse numbers.
data Exactness = E | I

-- Parses an integer from a string given a radix.
readInteger :: Num a => Int -> String -> a
readInteger r = readInteger' . reverse
  where
    r' = fromIntegral r
    readInteger' [] = 0
    readInteger' (x:xs) = x' + r' * readInteger' xs
      where
        x' = fromIntegral $ if isDigit x
                                then ord x - ord '0'
                                else ord (toLower x) - ord 'a' + 10

-- Attempt to parse a string without consuming any input.
tryString :: String -> Parser String
tryString = try . string

tokenize :: Parser [IntrikyToken]
tokenize = many $ try (intertokenSpace >> token)

token :: Parser IntrikyToken
token = choice
    [ identifier
    , boolean
    , liftM NumberTok number
    , character
    , string'
    , (char '('         >> return LParenTok)
    , (char ')'         >> return RParenTok)
    , (tryString "#("   >> return VectorTok)
    , (tryString "#u8(" >> return BytevectorTok)
    , (char '\''        >> return QuoteTok)
    , (char '`'         >> return BacktickTok)
    , (char ','         >> return CommaTok)
    , (tryString ",@"   >> return SeqCommaTok)
    , (char '.'         >> return DotTok)
    ]

delimiter :: Parser Char
delimiter = choice [whitespace, verticalLine, oneOf "()\";"]

intralineWhitespace :: Parser Char
intralineWhitespace = oneOf " \t"

whitespace :: Parser Char
whitespace = intralineWhitespace <|> lineEnding

verticalLine :: Parser Char
verticalLine = char '|'

lineEnding :: Parser Char
lineEnding = try $ choice
    [ newline
    , try (char '\r' >> newline)
    , char '\r'
    ] >> return '\n'

comment :: Parser ()
comment = choice
    [ try (char ';' >> manyTill anyChar lineEnding) >> return ()
    , nestedComment
--    , tryString "#;" >> intertokenSpace >> datum
    ] >> return ()

nestedComment :: Parser ()
nestedComment = try $ do
    _ <- tryString "#|"
    commentText
    _ <- many commentCont
    _ <- tryString "|#"
    return ()

commentText :: Parser ()
commentText = manyTill anyChar (tryString "#|" <|> tryString "|#") >> return ()

commentCont :: Parser ()
commentCont = nestedComment >> commentText

-- TODO: currently these are ignored.
directive :: Parser ()
directive = choice
    [ tryString "#!fold-case"
    , tryString "#!no-fold-case"
    ] >> return ()

atmosphere :: Parser ()
atmosphere = choice
    [ whitespace >> return ()
    , comment
    , try (directive >> intertokenSpace)
    ]

intertokenSpace :: Parser ()
intertokenSpace = many atmosphere >> return ()

identifier :: Parser IntrikyToken
identifier = choice [regular, vert, peculiarIdentifier]
  where
    regular = try $ do
        x <- initial
        xs <- many subsequent
        return $ IdentTok (x:xs)
    vert = try $ do
        _ <- verticalLine
        xs <- many symbolElement
        _ <- verticalLine
        return (IdentTok xs)

initial :: Parser Char
initial = letter <|> specialInitial

letter :: Parser Char
letter = oneOf $ ['a'..'z'] ++ ['A'..'Z']

specialInitial :: Parser Char
specialInitial = oneOf "!$%&*/:<=>?^_~"

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
inlineHexEscape = try $ do
    _ <- tryString "\\x"
    x <- hexScalarValue
    _ <- char ';'
    return (chr x)

hexScalarValue :: Parser Int
hexScalarValue = liftM (readInteger 16) (many1 hexDigit)

mnemonicEscape :: Parser Char
mnemonicEscape = choice
    [ tryString "\\a" >> return '\a'
    , tryString "\\b" >> return '\b'
    , tryString "\\t" >> return '\t'
    , tryString "\\n" >> return '\n'
    , tryString "\\r" >> return '\r'
    ]
 
peculiarIdentifier :: Parser IntrikyToken
peculiarIdentifier = liftM IdentTok $ choice [plusMinus, noDot, withDot]
  where
    plusMinus = liftM return explicitSign
    noDot = try $ do
        x <- explicitSign
        y <- signSubsequent
        zs <- many subsequent
        return (x:y:zs)
    withDot = try $ do
        x <- optionMaybe explicitSign
        _ <- char '.'
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
    , try (string "\\|" >> char '|')
    ]

boolean :: Parser IntrikyToken
boolean = liftM BoolTok (true <|> false)
  where
    true =  (tryString "#true" <|> tryString "#t")  >> return True
    false = (tryString "#false" <|> tryString "#f") >> return False

character :: Parser IntrikyToken
character = choice
    [ tryString "#\\" >> liftM CharTok anyChar
    , tryString "#\\" >> liftM CharTok characterName
    , tryString "#\\x" >> liftM (CharTok . chr) hexScalarValue
    ]

characterName :: Parser Char
characterName = choice
    [ tryString "alarm"     >> return '\a'
    , tryString "backspace" >> return '\b'
    , tryString "delete"    >> return '\x7f'
    , tryString "escape"    >> return '\x1b'
    , tryString "newline"   >> return '\n'
    , tryString "null"      >> return '\0'
    , tryString "return"    >> return '\r'
    , tryString "space"     >> return ' '
    , tryString "tab"       >> return '\t'
    ]

string' :: Parser IntrikyToken
string' = liftM StringTok $ between q q (many stringElement)
  where q = char '"'
 
stringElement :: Parser Char
stringElement = choice
    [ noneOf "\"\\"
    , mnemonicEscape
    , tryString "\\\"" >> return '"'
    , tryString "\\\\" >> return '\\'
    , try (char '\\' >> many intralineWhitespace >> lineEnding
                     >> many intralineWhitespace >> stringElement)
    , inlineHexEscape
    ]

-- Parses a number.
number :: Parser IntrikyNumber
number = choice [num 2, num 8, num 10, num 16]

-- Parses a number with a given radix.
num :: Int -> Parser IntrikyNumber
num r = do
    p <- prefix r
    (x, y) <- complex r
    case p of
        E -> return $ Number x y
        I -> return $ Number (toInexact x) (toInexact y)
  where
    toInexact (Exact n) = Inexact (fromIntegral x / fromIntegral y)
      where x = numerator n
            y = denominator n
    toInexact x = x

-- Parses a complex number with a given radix.
complex :: Int -> Parser (NumPart, NumPart)
complex r = choice [allReal, polar, special, others]
  where
    unit = oneOf "iI" >> return ()
    allReal = do
        x <- real r
        return (x, Exact 0)
    polar = try $ do
        _ <- real r
        _ <- char '@'
        _ <- real r
        return undefined -- TODO calculate polar stuff instead
    special = try $ do
        x <- option (Exact 0) (real r)
        y <- infnan
        unit
        return (x, y)
    others = try $ do
        x <- option (Exact 0) (real r)
        s <- sign
        y <- option 0 (ureal r)
        unit
        return (x, Exact (s y))

-- Parses a signed real or a special number.
real :: Int -> Parser NumPart
real r = choice
    [ try $ do s <- sign; x <- ureal r; return $ Exact (s x)
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
    ratioLiteral = try $ do
        x <- uinteger r
        _ <- char '/'
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
    fractional = try $ do
        _ <- char '.'
        digits <- many1 (digit' 10)
        let x = readInteger 10 digits
            y = 10 ^ (length digits)
        return (x % y)
    both = try $ do
        before <- many1 (digit' 10)
        _ <- char '.'
        after <- many (digit' 10)
        let x = readInteger 10 (before ++ after)
            y = 10 ^ (length after)
        return (x % y)
decimal _ = undefined

-- Parses an unsigned decimal integer.
uinteger :: Int -> Parser Integer
uinteger r = liftM (readInteger r) $ many1 (digit' r)

-- Parses radix and exactness prefixes.
prefix :: Int -> Parser Exactness
prefix r = choice
    [ radix r >> exactness
    , try $ do  x <- exactness; radix r; return x
    ]

-- Parses an infinity or NaN.
infnan :: Parser NumPart
infnan = liftM Special $ choice
    [ tryString "+inf.0" >> return PlusInf
    , tryString "-inf.0" >> return MinusInf
    , tryString "+nan.0" >> return PlusNan
    , tryString "-nan.0" >> return MinusNan
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
    i = try $ char '#' >> oneOf "iI" >> return I
    e = try $ char '#' >> oneOf "eE" >> return E

-- Parses a radix prefix.
radix :: Int -> Parser ()
radix 2 = tryString "#b" >> return ()
radix 8 = tryString "#o" >> return ()
radix 10 = optional (tryString "#d")
radix 16 = tryString "#d" >> return ()
radix _ = undefined

-- Parses a digit for a number with a given radix.
digit' :: Int -> Parser Char
digit' 2 = oneOf "01"
digit' 8 = oneOf ['0'..'7']
digit' 10 = digit
digit' 16 = hexDigit
digit' _ = undefined
