module Intriky.Types where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V

data IntrikyType
    = Boolean Bool
    | Bytevector B.ByteString
    | Char' Char
    | EofObject
    | Label LabelType
    | Null
    | Number
        { realPart          :: NumberPart
        , imagPart          :: NumberPart
        }
    | Port
    | Procedure
    | Pair
        { car               :: IntrikyType
        , cdr               :: IntrikyType
        }
    | Record
        { recordName        :: T.Text
        , recordConstructor :: [T.Text]
        , recordPredicate   :: T.Text
        , recordFields      :: [RecordField]
        }
    | Symbol T.Text
    | String' T.Text
    | Vector' (V.Vector IntrikyType)

data LabelType
    = WithLabel Integer IntrikyType
    | LabelRef Integer

data NumberPart
    = Exact Rational
    | Inexact Double
    | PlusInf
    | MinusInf
    | Nan

data RecordField = RecordField
    { fieldName :: T.Text
    , accessor  :: T.Text
    , modifier  :: T.Text
    }
