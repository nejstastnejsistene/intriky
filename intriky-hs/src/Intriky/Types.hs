module Intriky.Types where

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

class Eq a => IntrikyType a where
    -- Defines the behavior of the eq? function.
    eq    :: a -> a -> IntrikyBoolean
    -- Defines the behavior of the eqv? function.
    eqv   :: a -> a -> IntrikyBoolean
    -- Defines the behavior of the equal? function.
    equal :: a -> a -> IntrikyBoolean

    eq    = eqv
    equal = (==)


type IntrikyBoolean = Bool
instance IntrikyType IntrikyBoolean where
    eqv = (==)
-- Eq instance provided by Bool.


type IntrikyBytevector = B.ByteString
instance IntrikyType IntrikyBytevector where
    -- Because Haskell data are immutable, the notion of having the
    -- same location in memory is meaninless. See section 6.1.
    eqv _ _ = False
-- Recursive Eq instance kindly provided by Bytevector.


type IntrikyChar = Char
instance IntrikyType IntrikyChar where
    eqv = (==)
-- Eq instance provided by Char.


data IntrikyEofObject = EofObject deriving (Eq)
instance IntrikyType IntrikyEofObject where
    -- Singleton instance is always equal to itself.
    eqv _ _ = True


data IntrikyNull = Null deriving (Eq)
instance IntrikyType IntrikyNull where
    -- Singleton instance is always equal to itself.
    eqv _ _ = True


data SpecialNum = PlusInf | MinusInf | PlusNan | MinusNan deriving (Eq)
data NumPart
    = Exact Rational
    | Inexact Double
    | Special SpecialNum
    deriving (Eq)
data IntrikyNumber = Number
    { realPart :: NumPart
    , imagPart :: NumPart
    } deriving (Eq)
instance IntrikyType IntrikyNumber where
    eqv = undefined
-- TODO: (==)


data IntrikyPair a b = Pair a b
instance (IntrikyType a, IntrikyType b) => IntrikyType (IntrikyPair a b) where
    -- Because Haskell data are immutable, the notion of having the
    -- same location in memory is meaninless. See section 6.1.
    eqv _ _ = False
instance (IntrikyType a, IntrikyType b) => Eq (IntrikyPair a b) where
    -- Recursively apply (==).
    Pair x y == Pair x' y' = x == x' && y == y'


data IntrikyPort = Port deriving (Eq) -- TODO
instance IntrikyType IntrikyPort where
    eqv = undefined


data IntrikyProcedure = Procedure deriving (Eq) -- TODO
instance IntrikyType IntrikyProcedure where
    eqv = undefined


data IntrikyRecord = IntrikyRecord
    { recordName        :: T.Text
    , recordConstructor :: [T.Text]
    , recordPredicate   :: T.Text
    , recordFields      :: [RecordField]
    } deriving (Eq)
data RecordField = RecordField
    { fieldName :: T.Text
    , accessor  :: T.Text
    , modifier  :: T.Text
    } deriving (Eq)
instance IntrikyType IntrikyRecord where
    -- Because Haskell data are immutable, the notion of having the
    -- same location in memory is meaninless. See section 6.1.
    eqv _ _ = False



type IntrikyString = T.Text
instance IntrikyType IntrikyString where
    -- Because Haskell data are immutable, the notion of having the
    -- same location in memory is meaninless. See section 6.1.
    eqv _ _ = False
-- Eq instance defined by Text.


-- Symbols are essentiall strings in a different context. I wrapped this in 
-- a data type so the type system can tell them apart.
data IntrikySymbol = Symbol T.Text deriving (Eq)
instance IntrikyType IntrikySymbol where
    eqv = (==)
    

type IntrikyVector a = V.Vector a
instance IntrikyType a => IntrikyType (IntrikyVector a) where
    -- Because Haskell data are immutable, the notion of having the
    -- same location in memory is meaninless. See section 6.1.
    eqv _ _ = False
-- Recursive Eq instance kindly provided by Vector.
