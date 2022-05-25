module Eval where

import Control.Monad.Except (throwError)

import Apply
import Error
import Lisp

evalL :: Int -> LispVal -> ThrowsError LispVal
evalL l val | l < 0                = throwError $ Unquote val

evalL 0 (List [QuasiquoteP, val])  = evalL 1 val
evalL l (List [QuasiquoteP, val])  =
  (\v -> List [QuasiquoteP, v]) <$> evalL (l + 1) val
evalL _ (List (QuasiquoteP: args)) = throwError $ NumArgs 1 args

evalL 0 (List [QuoteP, val])       = return val
evalL l (List [QuoteP, val])       =
  (\v -> List [QuoteP,      v]) <$> evalL l val
evalL _ (List (QuoteP     : args)) = throwError $ NumArgs 1 args

evalL 1 (List [UnquoteP,    val])  = evalL 0 val
evalL l (List [UnquoteP,    val])  =
  (\v -> List [UnquoteP,    v]) <$> evalL (l - 1) val
evalL _ (List (UnquoteP   : args)) = throwError $ NumArgs 1 args

evalL 0 (List (Atom func : args))  = apply func args
evalL 0 val@(List _)               = throwError $ BadForm val
evalL l (List vals)                = List <$> mapM (evalL l) vals

evalL _ val = return val

eval :: LispVal -> ThrowsError LispVal
eval = evalL 0
