module Error where

import Control.Monad.Except          (ExceptT)
import Text.ParserCombinators.Parsec (ParseError)

import Lisp

data LispError
  = BadForm LispVal
  | NoFunction String
  | NumArgs Int [LispVal]
  | ParseErr ParseError
  | TypeError String LispVal
  | Unquote LispVal

instance Show LispError where
  show (BadForm     val) = "Unrecognized form: " ++ show val
  show (NoFunction  val) = "No such function: " ++ val
  show (NumArgs  n vals) =
    "Need " ++ show n ++ " arguments, but got " ++ show (List vals)
  show (ParseErr    err) = "Parser error: " ++ show err
  show (TypeError n val) = "Could not convert to " ++ n ++ ": " ++ show val
  show (Unquote     val) = "unquote not within quasiquote: " ++ show val

type ThrowsError = ExceptT LispError IO
