module Error where

import Text.ParserCombinators.Parsec (ParseError)

import Lisp

throwError :: a -> Either a b
throwError = Left

data LispError
  = BadForm LispVal
  | NoBool LispVal
  | NoFunction String
  | NoNumber LispVal
  | NumArgs Int LispVal
  | ParseErr ParseError
  | Unquote LispVal

instance Show LispError where
  show (BadForm    val) = "Unrecognized form: " ++ show val
  show (NoBool     val) = "Could not extract boolean from " ++ show val
  show (NoFunction val) = "No such function: " ++ val
  show (NoNumber   val) = "Could not extract number from " ++ show val
  show (NumArgs n  val) = "Need " ++ show n ++ " arguments, but got " ++ show val
  show (ParseErr   err) = "Parser error: " ++ show err
  show (Unquote    val) = "unquote not within quasiquote: " ++ show val

type ThrowsError = Either LispError
