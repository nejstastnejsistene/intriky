module Intriky.Parser where

import           Control.Monad   (liftM)
import qualified Data.ByteString as B
import           Data.Ratio      (numerator, denominator)
import qualified Data.Text       as T
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
    quoteAbbr = try $ do
        quoteToken
        x <- datum
        return $ toList [Symbol "quote", x]
    fullQuote = try . parens $ do
        quote <- symbol "quote"
        y <- datum
        return $ toList [quote, y]

procedureCall :: Parser IntrikyType
procedureCall = try . parens $ do
    x <- expression       -- operator
    ys <- many expression -- many operaand
    return $ toList (x:ys)

lambdaExpression :: Parser IntrikyType
lambdaExpression = try . parens $ do
    lambda <- symbol "lambda"
    x <- formals
    y <- body
    return $ toList [lambda, x, y]

formals :: Parser IntrikyType
formals = choice [regList, symbolToken, dottedList]
  where
    regList = try . parens $ liftM toList (many symbolToken)
    dottedList = try . parens $ do
        xs <- many1 symbolToken
        y <- symbolToken
        return (toList' xs y)
    
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
assignment = try . parens $ do
    set <- symbol "set!"
    x <- symbolToken
    y <- expression
    return $ toList [set, x, y]

definition :: Parser IntrikyType
definition = undefined
