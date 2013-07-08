module Intriky.Parser where

import Control.Monad
import qualified Data.ByteString as B
import Data.Char (chr, ord, isDigit, toLower)
import Data.Ratio
import Data.Word (Word8)
import Text.ParserCombinators.Parsec hiding (digit, letter, hexDigit)

import Intriky.Parser.Numbers (parseNumber)
import Intriky.Types

data IntrikyToken
    = IdentifierToken String
    | BooleanToken Bool
    | NumberToken IntrikyNumber
    | CharacterToken Char
    | StringToken String
    | RParenToken
    | LParenToken
    | VectorToken
    | BytevectorToken
    | QuoteToken
    | BacktickToken
    | CommaToken
    | SequenceCommaToken
    | DotToken

{- 7.1.1. Lexical structure -}

token' :: Parser IntrikyToken
token' =
      identifier
  <|> boolean
  <|> liftM NumberToken parseNumber
  <|> character
  <|> string'
  <|> (string "("    >> return LParenToken)
  <|> (string ")"    >> return RParenToken)
  <|> (string "#("   >> return VectorToken)
  <|> (string "#u8(" >> return BytevectorToken)
  <|> (string "'"    >> return QuoteToken)
  <|> (string "`"    >> return BacktickToken)
  <|> (string ","    >> return CommaToken)
  <|> (string ",@"   >> return SequenceCommaToken)
  <|> (string "."    >> return DotToken)

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
comment = (
      (char ';' >> manyTill anyChar lineEnding >> return ())
  <|> nestedComment
--  <|> string "#;" >> intertokenSpace >> datum
  ) >> return ()

nestedComment :: Parser ()
nestedComment
    = string "#|"
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
        return $ IdentifierToken (x:xs)
    vert = do
        verticalLine
        xs <- many symbolElement
        verticalLine
        return (IdentifierToken xs)

initial :: Parser Char
initial = letter <|> specialInitial

letter :: Parser Char
letter = oneOf $ ['a'..'z'] ++ ['A'..'Z']

specialInitial :: Parser Char
specialInitial = oneOf "~$%&*/:<=>?^_~"

subsequent :: Parser Char
subsequent = initial <|> digit <|> specialSubsequent

digit :: Parser Char
digit = oneOf ['0'..'9']

hexDigit :: Parser Char
hexDigit = digit <|> oneOf ['a'..'f'] <|> oneOf ['A'..'F']

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
mnemonicEscape =
      (string "\\a" >> return '\a')
  <|> (string "\\b" >> return '\b')
  <|> (string "\\t" >> return '\t')
  <|> (string "\\n" >> return '\n')
  <|> (string "\\r" >> return '\r')
 
peculiarIdentifier :: Parser IntrikyToken
peculiarIdentifier = fmap IdentifierToken $ choice [plusMinus, noDot, withDot]
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
dotSubsequent = signSubsequent <|> char '.'

signSubsequent :: Parser Char
signSubsequent = initial <|> explicitSign <|> char '@'

symbolElement :: Parser Char
symbolElement =
      noneOf "|\\"
  <|> inlineHexEscape
  <|> mnemonicEscape
  <|> (string "\\|" >> mzero)

boolean :: Parser IntrikyToken
boolean = fmap BooleanToken $ choice [t, f]
  where
    t = choice [string "#t", string "#true"]  >> return True
    f = choice [string "#f", string "#false"] >> return False

character :: Parser IntrikyToken
character =
      (string "#\\" >> anyChar >>= return . CharacterToken)
  <|> (string "#\\" >> characterName >>= return . CharacterToken)
  <|> (string "#\\x" >> hexScalarValue >>= return . CharacterToken . chr)

characterName :: Parser Char
characterName =
      (string "alarm"     >> return '\a')
  <|> (string "backspace" >> return '\b')
  <|> (string "delete"    >> return '\x7f')
  <|> (string "escape"    >> return '\x1b')
  <|> (string "newline"   >> return '\n')
  <|> (string "null"      >> return '\0')
  <|> (string "return"    >> return '\r')
  <|> (string "space"     >> return ' ')
  <|> (string "tab"       >> return '\t')

string' :: Parser IntrikyToken
string' = between q q (many stringElement) >>= return . StringToken
  where q = char '"'
 
stringElement :: Parser Char
stringElement =
      noneOf "\"\\"
  <|> mnemonicEscape
  <|> (string "\\\"" >> return '"')
  <|> (string "\\\\" >> return '\\')
  <|> (char '\\' >> many intralineWhitespace >> lineEnding
                 >> many intralineWhitespace >> mzero)
  <|> inlineHexEscape

{-
bytevector :: Parser IntrikyBytevector 
bytevector = do
    token BytevectorToken
    xs <- many (token NumberToken)
    token RParenToken
    return (B.pack xs)

byte :: Parser Word8
byte = choice (map (string . show) [0..255]) >>= read
-}
{- 7.1.2. External representations -}

datum =
      simpleDatum
  <|> compoundDatum
  <|> (label' >> token "=" >> datum)
  <|> (label' >> token "#")
simpleDatum =
      boolean
  <|> number
  <|> character
  <|> string
  <|> symbol
  <|> bytevector
symbol = identifier
compoundDatum = list <|> vector <|> abbreviation
list =
      (token "(" >> many datum  >> token ")")
  <|> (token "(" >> many1 datum >> token "." >> datum >> token ")")
abbreviation = abbrevPrefix >> datum
abbrevPrefix = oneOf "`," <|> string ",@"
vector = token "#(" >> many datum >> token ")"
label' = token "#" >> uinteger 10

{- 7.1.3 Expressions -}

{-
expression = identifier
         <|> literal
         <|> procedureCall
         <|> lambdaExpression
         <|> conditional
         <|> assignment
         <|> derivedExpression
         <|> macroUse
         <|> macroBlock
         <|> includer
literal = quotation <|> selfEvaluating
selfEvaluating = boolean
             <|> number
             <|> vector
             <|> character
             <|> string
             <|> bytevector
quotation = (char "'" >> datum) <|> (string "(quote" >> datum >> ")")
procedureCall = char "(" >> operator >> many operand >> char ")"
operator = expression
operand = expression
lambdaExpression = char "(lambda" >> formals >> body >> char ")"
formals = (char "(" >> many identifier >> char ")")
      <|> identifier
      <|> (char "(" many1 identifier >> char "." >> identifier >> char ")")
body = many definition >> sequence'
sequence' = many command >> expression
command = expression
conditional = string "(if" >> test >> consequent >> alternate >> char ")"
test = expression
consequent = expression
alternate = expression >> empty
assignment = string "(set!" >> identifier >> expression >> char ")"
{-
derivedExpression =
        string "(cond" >> many1 condClause >> char ")"
    <|> string "(cond" >> many condClause
                       >> string "(else" >> sequence' >> string "))"
    <|> string "(case" >> expression >> many1 caseClause >> char ")"
    <|> string "(case" >> expression >> many caseClause 
                       >> string "(else" >> optional (string "=>")
                       >> sequence' >> char "))"
    <|> string "(and" >> many test >> char ")"
    <|> string "(or" >> many test >> char ")"
    <|> string "(when" >> test >> sequence' >> char ")"
    <|> string "(unless" >> test >> sequence' >> char ")"
    <|> string "(let" >> optional identifier
                      >> char "(" >> many bindingSpec >> char ")"
                      >> body >> char ")"
    <|> string "(let*" >> char "(" >> many bindingSpec >> char ")"
                       >> body >> char ")"
    <|> string "(letrec" >> optional (char "*") >> char "("
                         >> many bindingSpec >> char ")" >> body >> char ")"
    <|> string "(let-values" >> optional (char "*") >> char "("
                >> many mvBindingSpec >> char ")" >> body >> char ")"
    <|> string "(begin" >> sequence' >> char ")"
    <|> string "(do" >> char "(" >> many iterationSpec >> char ")"
                    >> char "(" >> test >> doResult >> char ")"
                    >> many command >> char ")"
    <|> string "(delay" >> expression >> char ")"
    <|> string "(delay-force" >> expression >> char ")"
    <|> string "(parameterize" >> char "("
            >> many (char "(" >> expression >> expression >> char ")")
            >> char ")" >> body >> char ")"
    <|> string "(guard" >> char "(" >> identifier >> many condClause
        >> char ")" >> body >> char ")"
    <|> quasiquotation
    <|> string "(case-lambda" >> many caseLambdaClause >> char ")"
-}
-}
-}
