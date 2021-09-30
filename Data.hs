{-# LANGUAGE FlexibleInstances #-} 

module Data where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import System.IO
import Data.IORef
import Control.Monad.IO.Class


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
