module Main (main) where

import Control.Monad.Except (liftIO, runExceptT)

import Apply
import Eval
import Read  (readExpr)
import Types
import Utils

repl :: Lisp
repl = readExpr "(loop (try (print (eval (block (out \"Lisp> \") (read)))) outL))"

test :: Lisp
test = liftIO (readFile "test") >>= readExpr

main :: IO ()
main = runExceptT (runStateTA (runStateTA
  (repl >>= eval) (L clean)) (G clean)) >>=
  flushStr . (++ "\n") <$> either (("ERROR: " ++) . show) show
