import Text.ParserCombinators.Parsec (parse)
import Intriky.Lexer (tokenize)

main :: IO ()
main = do
    life <- readFile "../life.scm"
    case parse (tokenize) "" life of
        Left err -> putStrLn (show err)
        Right x -> print x
