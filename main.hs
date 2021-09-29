
module Main where 
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad


-- Data Types

data LispVal = Atom String -- which stores a String naming the atom
             | List [LispVal] -- which stores a list of other LispVals
             | DottedList [LispVal] LispVal -- improper list
             | Number Integer -- containing a Haskell Integer
             | Float Float -- containing a Haskell float
             | String String -- containing a Haskell String
             | Bool Bool -- containing a Haskell boolean value

-- Parser

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              -- let atom = [first] ++ rest
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  x <- many1 digit
  return $ Number $ read x
-- parseNumber = liftM (Number . read) $ many1 digit

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  let atom = (x ++ "." ++ y)
  return $ Float $ read atom

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> try parseFloat
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

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

spaces :: Parser()
spaces = skipMany1 space -- ignore spaces

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" -- recognizes symbols

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
