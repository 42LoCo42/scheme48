module Apply where

import Text.Read (readMaybe)

import Error
import Lisp

fill :: a -> [a] -> [a]
fill a [] = [a]
fill _ as = as

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
  (throwError $ NoFunction func)
  ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numeric 0 sum)
  , ("-", numeric 0 $ foldl1 (-))
  , ("*", numeric 1 product)
  , ("/", numeric 1 $ foldl1 (/))
  ]

toNumber :: LispVal -> ThrowsError Double
toNumber (Number n)   = return n
toNumber s@(String n) = maybe (throwError $ NoNumber s) return $ readMaybe n
toNumber (List (n:_)) = toNumber n
toNumber (Bool True)  = return 1
toNumber (Bool False) = return 0
toNumber e            = throwError $ NoNumber e

toBool :: LispVal -> ThrowsError Bool
toBool (Bool b)         = return b
toBool (Atom "true")    = return True
toBool (Atom "false")   = return False
toBool (String "true")  = return True
toBool (String "false") = return False
toBool (List (b:_))     = toBool b
toBool (Number 0.0)     = return False
toBool (Number _)       = return True
toBool e                = throwError $ NoBool e

numeric :: Double -> ([Double] -> Double) -> [LispVal] -> ThrowsError LispVal
numeric empty func args = Number . func . fill empty <$> mapM toNumber args
