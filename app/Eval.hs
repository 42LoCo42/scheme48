module Eval where

import Control.Monad.Except (throwError)
import Control.Monad.State  (gets, lift)
import Data.Map.Strict      (union, (!?))

import Apply
import Types

quoteErr :: [LispVal] -> Lisp
quoteErr = throwError . NumArgs [Any "expr"]

evalL :: Int -> LispVal -> Lisp
evalL l val | l < 0                = throwError $ Unquote val

evalL 0 (List [QuasiquoteP, val])  = evalL 1 val
evalL l (List [QuasiquoteP, val])  =
  (\v -> List [QuasiquoteP, v]) <$> evalL (l + 1) val
evalL _ (List (QuasiquoteP: args)) = quoteErr args

evalL 0 (List [QuoteP, val])       = return val
evalL l (List [QuoteP, val])       =
  (\v -> List [QuoteP,      v]) <$> evalL l val
evalL _ (List (QuoteP     : args)) = quoteErr args

evalL 1 (List [UnquoteP,    val])  = evalL 0 val
evalL l (List [UnquoteP,    val])  =
  (\v -> List [UnquoteP,    v]) <$> evalL (l - 1) val
evalL _ (List (UnquoteP   : args)) = quoteErr args

evalL 0 (List (Atom func : args))  = apply func args
evalL 0 val@(List _)               = throwError $ BadForm val
evalL l (List vals)                = List <$> mapM (evalL l) vals

evalL 0 (Atom s)                   = do
  gVars <- lift $ gets (fst . global)
  lVars <- gets (fst . local)
  maybe (throwError $ NoVariable s) return $ (lVars `union` gVars) !? s

evalL _ val = return val

eval :: LispVal -> Lisp
eval = evalL 0
