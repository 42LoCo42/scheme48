module Main (main) where

import Control.Monad.Except (runExceptT)

import Eval
import Lisp
import Utils

repl :: LispVal
repl =
  List [Atom "loop",
  List [Atom "print",
  List [Atom "eval",
  List [Atom "block",
    List [Atom "out", String "Lisp> "],
    List [Atom "read"]
  ]]]]

main :: IO ()
main = runExceptT (eval repl) >>= either
  (return . ("ERROR: " ++ ) . show)
  (return . show) >>= (flushStr . (++ "\n"))
