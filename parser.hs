module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readOct, readHex)

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do char '('
       x <- try parseList <|> parseDottedList
       char ')'
       return x

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChars <|> noneOf ['\\', '"'])
  char '"'
  return $ String x

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  c <- oneOf ['\\','"', 'n', 'r', 't']
  return $ case c of
    '\\' -> c
    '"'  -> c
    'n'  -> '\n'
    't'  -> '\t'
    'r'  -> '\r'

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseStdNumber <|> parseRadixNumber

parseStdNumber :: Parser LispVal
parseStdNumber = fmap (Number . read) $ many1 digit

parseRadixNumber :: Parser LispVal
parseRadixNumber = do
  char '#' >> (parseDecimal <|> parseHex <|> parseBinary <|> parseOctal)

parseDecimal :: Parser LispVal
parseDecimal = do
  char 'd'
  n <- many1 digit
  (return . Number . read) n

parseHex :: Parser LispVal
parseHex = do
  char 'x'
  n <- many $ oneOf "0123456789abcdefABCDEF"
  (return . Number . (rd readHex)) n

parseBinary :: Parser LispVal
parseBinary = do
  char 'b'
  n <- many $ oneOf "01"
  (return . Number . binToInt) n

binToInt :: String -> Integer
binToInt s = sum $ map (\(i,x) -> i*(2^x)) $ zip [0..] $ map p (reverse s)
  where p '0' = 0
        p '1' = 1

parseOctal :: Parser LispVal
parseOctal = do
  char 'o'
  n <- many $ oneOf "01234567"
  (return . Number . (rd readOct)) n

rd f s = fst $ head (f s)



-- Recursive Parsers

parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
