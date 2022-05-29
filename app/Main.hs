module Main (main) where

import Control.Monad.Except (liftIO, runExceptT)

import Apply
import Eval
import Read  (readExpr)
import Types
import Utils

repl :: Lisp
repl = readExpr "(loop (print (eval (block (out \"Lisp> \") (read)))))"

test :: Lisp
test = liftIO (readFile "test") >>= readExpr

main :: IO ()
main = runExceptT (runStateTA (runStateTA
  (test >>= eval) (L clean)) (G clean)) >>=
  flushStr . (++ "\n") <$> either (("ERROR: " ++) . show) show
