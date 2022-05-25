module Eval where

import Error
import Lisp

eval :: LispVal -> ThrowsError LispVal
