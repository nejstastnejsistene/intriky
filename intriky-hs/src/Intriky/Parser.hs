module Intriky.Parser where

import qualified Data.ByteString as B
import           Data.Ratio      (numerator, denominator)
import           Data.Vector     (fromList)
import           Data.Word       (Word8)
import           Text.ParserCombinators.Parsec hiding (label)

import Intriky.Lexer
import Intriky.Types

parens :: Parser a -> Parser a
parens = between lParenToken rParenToken

toList :: [IntrikyType] -> IntrikyType
toList xs = toList' xs Null

toList' :: [IntrikyType] -> IntrikyType -> IntrikyType
toList' xs y = foldr Pair y xs

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
    byte' _ = fail $ err ++ " (inexact or complex)"

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
    normalList = parens $ do
        xs <- many datum
        return (toList xs)
    dottedList = parens . try $ do
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
abbrevPrefix = choice
    [ quoteToken    >> return (Symbol "quote")
    , backtickToken >> return (Symbol "quasiquote")
    , commaToken    >> return (Symbol "unquote")
    , seqCommaToken >> return (Symbol "unquote-splicing")
    ]

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
--    , lambdaExpression
--    , conditional
--    , assignment
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
    quote x = toList [Symbol "quote", x]
    quoteAbbr = try $ do
        quoteToken
        x <- datum
        return (quote x)
    fullQuote = try $ do
        lParenToken
        x <- symbolToken
        case x of
            (Symbol "quote") -> do
                y <- datum
                rParenToken
                return (quote y)
            _ -> fail "expecting (Symbol \"quote\")"

procedureCall :: Parser IntrikyType
procedureCall = try $ do
    lParenToken
    x <- expression
    ys <- many expression
    rParenToken
    return $ toList (x:ys)
