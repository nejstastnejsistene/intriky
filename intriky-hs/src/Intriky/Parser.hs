module Intriky.Parser where

import           Control.Monad   (liftM, liftM2, replicateM)
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

toListM :: Monad m => m [IntrikyType] -> m IntrikyType
toListM = liftM toList

-- Parse a list of parsers into a Scheme list.
seqList :: [Parser IntrikyType] -> Parser IntrikyType
seqList = parens . seqList'

seqList' :: [Parser IntrikyType] -> Parser IntrikyType
seqList' = try . toListM . sequence

option' :: Parser a -> Parser [a]
option' = liftM (maybe [] return) . optionMaybe

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
    normalList = try . parens $ toListM (many datum)
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
    , derivedExpression
    , macroUse
    , macroBlock
    , includer
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
quotation = choice
    [ seqList' [quoteToken, datum]
    , seqList [symbol "quote", datum]
    ]

procedureCall :: Parser IntrikyType
procedureCall = try $ toListM (many1 expression)

lambdaExpression :: Parser IntrikyType
lambdaExpression = seqList [symbol "lambda", formals, body]

formals :: Parser IntrikyType
formals = choice [regList, symbolToken, dottedList]
  where
    regList = try . parens $ toListM (many symbolToken)
    dottedList = try . parens $ liftM2 toList' (many1 symbolToken) symbolToken
    
body :: Parser IntrikyType
body = try $ do
    xs <- many definition
    y <- sequence'
    return $ toList (xs ++ [y])

sequence' :: Parser IntrikyType
sequence' = try $ do
    xs <- many expression
    y <- expression
    return $ toList (xs ++ [y])

conditional :: Parser IntrikyType
conditional = try . parens $ do
    if' <- symbol "if"
    x <- expression
    y <- expression
    z <- option' expression
    return (toList $ [if', x, y] ++ z)

assignment :: Parser IntrikyType
assignment = seqList [symbol "set!", symbolToken, expression]

derivedExpression :: Parser IntrikyType
derivedExpression = choice $ quasiquotation : map (try . toListM)
    [ do x <- symbol "cond"
         xs <- many1 condClause 
         return (x:xs)
    , do x <- symbol "cond"
         xs <- many condClause
         e <- elseClause
         return (x:xs ++ [e])
    , do x <- symbol "case"
         y <- expression
         zs <- many1 caseClause
         return (x:y:zs)
    , do x <- symbol "case"
         y <- expression
         zs <- many caseClause
         e <- elseClause <|> elseRecipient
         return (x:y:zs ++ [e])
    , liftM2 (:) (symbol "and") (many expression)
    , liftM2 (:) (symbol "or") (many expression)
    , sequence [symbol "when", expression, sequence']
    , sequence [symbol "unless", expression, sequence']
    , sequence [lets, toListM (many bindingSpec), body]
    , sequence [symbol "let", symbolToken, toListM (many bindingSpec), body]
    , sequence [letValues, toListM (many mvBindingSpec), body]
    , sequence [symbol "begin", sequence']
    , do x <- symbol "do"
         y <- toListM (many iterationSpec)
         z <- toListM $ liftM2 (:) expression (option' sequence')
         zs <- many expression
         return (x:y:z:zs)
    , sequence [delays, expression]
    , sequence [symbol "parameterize", toListM (many exprPair), body]
    , sequence [symbol "guard", toListM (many guardSpec), body]
    , liftM2 (:) (symbol "case-lambda") (many caseLambdaClause)
    ] 
  where
    elseClause = seqList [symbol "else", sequence']
    elseRecipient = seqList [symbol "else", symbol "=>", expression]
    lets = choice $ map symbol ["let", "let*", "letrec", "letrec*"]
    letValues = symbol "let-values" <|> symbol "let*-values"
    delays = symbol "delay" <|> symbol "delay-force"
    exprPair = seqList [expression, expression]
    guardSpec = toListM $ liftM2 (:) symbolToken (many condClause)

condClause :: Parser IntrikyType
condClause = choice $ map seqList
    [ [expression, sequence']
    , [expression]
    , [expression, symbol "=>", expression]
    ]

caseClause :: Parser IntrikyType
caseClause = choice $ map seqList
    [ [toListM (many datum), sequence']
    , [toListM (many datum), symbol "=>", expression]
    ]

bindingSpec :: Parser IntrikyType
bindingSpec = seqList [symbolToken, expression]

mvBindingSpec :: Parser IntrikyType
mvBindingSpec = seqList [formals, expression]

iterationSpec :: Parser IntrikyType
iterationSpec = toListM $ do
    x <- symbolToken
    y <- expression
    z <- option' expression
    return (x:y:z)

caseLambdaClause :: Parser IntrikyType
caseLambdaClause = seqList [formals, body]

macroUse :: Parser IntrikyType
macroUse = toListM $ liftM2 (:) symbolToken (many datum)

macroBlock :: Parser IntrikyType
macroBlock = seqList [letSyntax, toListM (many syntaxSpec), body]
  where
    letSyntax = symbol "let-syntax" <|> symbol "letrec-syntax"

syntaxSpec :: Parser IntrikyType
syntaxSpec = seqList [symbolToken, transformerSpec]

includer :: Parser IntrikyType
includer = toListM $ liftM2 (:) include (many1 stringToken)
  where
    include = symbol "include" <|> symbol "include-ci"

{- 7.1.4 Quasiquotations -}

quasiquotation :: Parser IntrikyType
quasiquotation = quasiquotation' 1

quasiquotation' :: Int -> Parser IntrikyType
quasiquotation' d = choice
    [ seqList' [backtickToken, qqTemplate d]
    , seqList [symbol "quasiquote", qqTemplate d]
    ]

qqTemplate :: Int -> Parser IntrikyType
qqTemplate 0 = expression
qqTemplate d = choice
    [ simpleDatum
    , listQQTemplate d
    , vectorQQTemplate d
    , unquotation d
    ]

listQQTemplate :: Int -> Parser IntrikyType
listQQTemplate d = choice
    [ toListM $ many (qqTemplateOrSplice d)
    , try . parens $ do xs <- many1 (qqTemplateOrSplice d)
                        dotToken
                        y <- qqTemplate d
                        return $ toList' xs y
    , seqList' [backtickToken, qqTemplate d]
    , quasiquotation' (d + 1)
    ]

vectorQQTemplate :: Int -> Parser IntrikyType
vectorQQTemplate d = try $ do
    vectorToken
    xs <- many (qqTemplateOrSplice d)
    rParenToken
    return $ Vector' (fromList xs)

unquotation :: Int -> Parser IntrikyType
unquotation d = choice
    [ seqList' [commaToken, qqTemplate (d - 1)]
    , seqList [symbol "unquote", qqTemplate (d - 1)]
    ]

qqTemplateOrSplice :: Int -> Parser IntrikyType
qqTemplateOrSplice d = qqTemplate d <|> splicingUnquotation d

splicingUnquotation :: Int -> Parser IntrikyType
splicingUnquotation d = choice
    [ seqList' [seqCommaToken, qqTemplate (d - 1)]
    , seqList [symbol "unquote-splicing", qqTemplate (d - 1)]
    ]

{- 7.1.5 Transformers -}

transformerSpec :: Parser IntrikyType
transformerSpec = undefined

{- 7.1.6 Programs and definitions -}

program :: Parser [IntrikyType]
program = liftM2 (++) (many1 importDeclaration) (many1 commandOrDefinition)

commandOrDefinition :: Parser IntrikyType
commandOrDefinition = choice
    [ expression
    , definition
    , toListM $ liftM2 (:) (symbol "begin") (many1 commandOrDefinition)
    ]

definition :: Parser IntrikyType
definition = choice
    [ seqList [symbol "define", symbolToken, expression]
    , seqList [symbol "define", liftM2 Pair symbolToken defFormals, body]
    , syntaxDef
    , seqList [symbol "define-values", formals, body]
    , do x <- symbol "define-record-type"
         ys <- sequence [symbolToken, constructor, symbolToken]
         zs <- many fieldSpec
         return $ toList (x:ys ++ zs)
    , toListM $ liftM2 (:) (symbol "begin") (many fieldSpec)
    ]

defFormals :: Parser IntrikyType
defFormals = choice
    [ toListM (many symbolToken)
    , do xs <- many symbolToken
         dotToken
         y <- symbolToken
         return $ toList' xs y      
    ]

constructor :: Parser IntrikyType
constructor = toListM $ liftM2 (:) symbolToken (many symbolToken)

fieldSpec :: Parser IntrikyType
fieldSpec = choice $ map toListM
    [ replicateM 2 symbolToken
    , replicateM 3 symbolToken
    ]

syntaxDef :: Parser IntrikyType
syntaxDef = seqList [symbol "define-syntax", symbolToken, transformerSpec]

{- 7.1.7 Libraries -}

importDeclaration :: Parser IntrikyType
importDeclaration = undefined
