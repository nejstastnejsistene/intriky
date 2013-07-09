module Intriky.Parser where

import qualified Data.ByteString as B
import           Data.Ratio      (numerator, denominator)
import           Data.Word       (Word8)
import           Text.ParserCombinators.Parsec

import Intriky.Lexer
import Intriky.Types

-- Parse a bytevector.
bytevector :: Parser IntrikyBytevector
bytevector = try $ do
    bytevectorToken
    xs <- many byte
    rParenToken
    return (B.pack xs)

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
