import Data.ByteString (unpack)
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
        Right (Bytevector x) -> putStrLn $ map (chr . fromIntegral) (unpack x)
        _ -> putStrLn "non-exhaustive pattern match blebleble"
