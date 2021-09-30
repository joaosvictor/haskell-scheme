{-# LANGUAGE FlexibleInstances #-} 

module Main where 
-- import Data
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import System.IO
import Data.IORef
import Control.Monad.IO.Class



-- Environment

type Env = IORef [(String, IORef LispVal)]

instance Show (IORef a) where
    show _ = "<ioref>"

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False
  (const True) . lookup var

getVar :: Env -> String -> IO LispVal
getVar envRef var = do
  env <- readIORef envRef
  maybe undefined 
        readIORef
        (lookup var env)

defineVar :: Env -> String -> LispVal -> IO LispVal
defineVar envRef var value = do
  valueRef <- newIORef value
  env <- readIORef envRef
  writeIORef envRef ((var, valueRef) : env)
  return value

-- Data Types

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
             deriving Show

instance Show ([LispVal] -> LispVal) where
    show _ = "<primitive>"

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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val  

-- Evaluation

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

unpackStr :: LispVal -> String
unpackStr (String s) = s

unpackAtom :: LispVal -> String
unpackAtom (Atom s) = s

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

compareBinop [Atom x, Atom y] = (Bool (x==y))
compareBinop _ = (Bool False)

boolBinop :: (LispVal -> a) -> (a -> a -> Bool) -> [LispVal] -> LispVal
boolBinop unpacker op [x, y] = Bool $ (unpacker x) `op` (unpacker y)

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

apply :: LispVal -> [LispVal] -> IO LispVal 
apply (PrimitiveFunc func) args = return $ func args
apply (Func params vararg body closure) args = do
  mapM (bindVar closure) (zip params args)
  eval closure (body !! 0)
  where bindVar env (p, arg) = defineVar env p arg

primitiveBindings :: IO Env
primitiveBindings = do
  env <- nullEnv
  mapM (makePrimitiveFunc env) primitives
  return env
  where makePrimitiveFunc env (var, func) = defineVar env var (PrimitiveFunc func)

makeFunc varargs env params body = return $ Func (map unpackAtom params) varargs body env
makeNormalFunc = makeFunc Nothing

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("eq?", compareBinop)]

eval :: Env -> LispVal -> IO LispVal
eval _ val@(Float _) = return val
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ (List [Atom "quote", val]) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "define", Atom var, form]) = 
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    otherwise -> eval env conseq
eval env (List (Atom "cond" : pairs)) = evalCond pairs
    where evalCond (List [Atom "else", value] : []) = eval env value
          evalCond (List [condition, value] : rest) = do
            conditionResult <- eval env condition
            case conditionResult of
                Bool False -> evalCond rest
                _ -> eval env value
          evalCond [] = pure $ Atom ""
eval env (List (Atom func : args)) = do
 x <- mapM (eval env) args
 f <- (getVar env func)
 apply f x

-- REPL

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalString :: Env -> String -> IO String
evalString env expr = liftM show $ eval env $ readExpr expr

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

main :: IO ()
main = do
  env <- primitiveBindings
  until_ (== "quit") (readPrompt "Lisp>>> ") (evalAndPrint env)
