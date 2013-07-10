import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Char (chr)
import Text.ParserCombinators.Parsec (parse)

import Intriky.Parser
import Intriky.Types

main :: IO ()
main = do
--    life <- readFile "../life.scm"
    let test = "#u8(72 101 108 108 111 44 32 119 111 114 108 100 33)"
    case parse datum "" test of
        Left err -> putStrLn (show err)
        Right (Bytevector x) -> putStrLn $ map (chr . fromIntegral) (B.unpack x)
        _ -> putStrLn "non-exhaustive pattern match blebleble"

    let test2 = "'ahoj"
    case parse quotation "" test2 of
        Left err -> putStrLn (show err)
        Right (Pair (Symbol x) (Pair (Symbol y) Null))
            -> putStrLn $ "(" ++ T.unpack x ++ " " ++ T.unpack y ++ ")"
        _ -> putStrLn "non-exhaustive pattern match blebleble"
