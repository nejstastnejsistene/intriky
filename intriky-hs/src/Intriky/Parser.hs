module Intriky.Parser where

import           Control.Monad   (liftM, liftM2)
import qualified Data.ByteString as B
import           Data.Ratio      (numerator, denominator)
import qualified Data.Text       as T
import           Data.Vector     (fromList)
import           Data.Word       (Word8)
import           Text.ParserCombinators.Parsec hiding (label)

import Intriky.Lexer
import Intriky.Types

-- Wrap something in parentheses.
parens :: Parser a -> Parser a
parens = between lParenToken rParenToken

-- Convert a Haskell list into a Scheme list.
toList :: [IntrikyType] -> IntrikyType
toList xs = toList' xs Null

-- Convert a Haskell list into a Scheme list with an explicit final value.
toList' :: [IntrikyType] -> IntrikyType -> IntrikyType
toList' xs y = foldr Pair y xs

-- Parse a list of parsers into a Scheme list.
seqList :: [Parser IntrikyType] -> Parser IntrikyType
seqList = liftM toList . sequence

-- Match a symbol of the given value.
symbol :: T.Text -> Parser IntrikyType
symbol x = do
    sym <- symbolToken
    case sym of
        Symbol y -> if x == y then return sym else fail err
        _ -> fail err
  where
    err = "expecting (Symbol \"" ++ T.unpack x ++ "\")"

-- Parse a bytevector.
bytevector :: Parser IntrikyType
bytevector = try $ do
    bytevectorToken
    xs <- many byte
    rParenToken
    return $ Bytevector (B.pack xs)

-- Any exact integer between 0 and 255.
byte :: Parser Word8
byte = numberToken >>= byte'
  where
    err = "expecting exact integer between 0 and 255"
    byte' (Number (Exact x) (Exact 0))
        | q == 1 && 0 <= p && p < 256 = return (fromInteger p)
        | otherwise = fail err
      where p = numerator x
            q = denominator x
    byte' _ = fail err

datum :: Parser IntrikyType
datum = simpleDatum <|> compoundDatum <|> withLabel <|> labelRef
  where
    withLabel = try $ do
        x <- label
        _ <- char '='
        y <- datum
        return $ Label (WithLabel x y)
    labelRef = try $ do
        x <- label
        _ <- char '#'
        return $ Label (LabelRef x)

simpleDatum :: Parser IntrikyType
simpleDatum = choice
    [ boolToken
    , numberToken
    , charToken
    , stringToken
    , symbolToken
    , bytevector
    ]

compoundDatum :: Parser IntrikyType
compoundDatum = list <|> vector <|> abbreviation

list :: Parser IntrikyType
list = normalList <|> dottedList
  where
    normalList = try . parens $ liftM toList (many datum)
    dottedList = try . parens $ do
        items <- many1 datum
        dotToken
        final <- datum
        return (toList' items final)

abbreviation :: Parser IntrikyType
abbreviation = try $ do
    x <- abbrevPrefix
    y <- datum
    return $ toList [x, y]

abbrevPrefix :: Parser IntrikyType
abbrevPrefix = choice [quoteToken, backtickToken, commaToken, seqCommaToken]

vector :: Parser IntrikyType
vector = try $ do
    vectorToken
    xs <- many datum
    rParenToken
    return $ Vector' (fromList xs)

label :: Parser Integer
label = try (char '#' >> uinteger 10)

{- 7.1.3. Expressions -}

expression :: Parser IntrikyType
expression = choice
    [ symbolToken
    , literal
    , procedureCall
    , lambdaExpression
    , conditional
    , assignment
--    , derivedExpression
--    , macroUse
--    , macroBlock
--    , includer
    ]

literal :: Parser IntrikyType
literal = quotation <|> selfEvaluating

selfEvaluating :: Parser IntrikyType
selfEvaluating = choice
    [ boolToken
    , numberToken
    , vector
    , charToken
    , stringToken
    , bytevector
    ]

quotation :: Parser IntrikyType
quotation = quoteAbbr <|> fullQuote
  where
    quoteAbbr = try $ seqList [quoteToken, datum]
    fullQuote = try . parens $ seqList [symbol "quote", datum]

procedureCall :: Parser IntrikyType
procedureCall = try . parens $ liftM toList (many1 expression)

lambdaExpression :: Parser IntrikyType
lambdaExpression = try . parens $ seqList [symbol "lambda", formals, body]

formals :: Parser IntrikyType
formals = choice [regList, symbolToken, dottedList]
  where
    regList = try . parens $ liftM toList (many symbolToken)
    dottedList = try . parens $ liftM2 toList' (many1 symbolToken) symbolToken
    
body :: Parser IntrikyType
body = try $ do
    xs <- many definition
    y <- sequence'
    return $ toList (xs ++ [y])

sequence' :: Parser IntrikyType
sequence' = try $ do
    xs <- many expression -- many command
    y <- expression
    return $ toList (xs ++ [y])

conditional :: Parser IntrikyType
conditional = try . parens $ do
    if' <- symbol "if"
    x <- expression -- test
    y <- expression -- consequent
    z <- alternate
    return (toList $ [if', x, y] ++ z)
  where
    alternate = liftM (maybe [] return) (optionMaybe expression)

assignment :: Parser IntrikyType
assignment = try . parens $ seqList [symbol "set!", symbolToken, expression]

definition :: Parser IntrikyType
definition = undefined
