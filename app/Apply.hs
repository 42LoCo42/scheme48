{-# LANGUAGE LambdaCase #-}
module Apply where

import Control.Monad.Except (liftIO, throwError)
import Text.Read            (readMaybe)

import                Error
import {-# SOURCE #-} Eval  (eval)
import                Lisp
import                Read
import                Utils

type ApplyBase = [LispVal] -> ThrowsError LispVal

data Apply
  = Function ApplyBase
  | Macro    ApplyBase

runApply :: Apply -> ApplyBase
runApply (Function f) args = mapM eval args >>= f
runApply (Macro    m) args = m args

apply :: String -> ApplyBase
apply name args = do
  case lookup name primitives of
    (Just func) -> runApply func args
    Nothing     -> throwError $ NoFunction name

primitives :: [(String, Apply)]
primitives =
  [ ("apply", applyFunc)
  , ("read",  readFunc)
  , ("eval",  evalFunc)
  , ("print", printFunc)
  , ("loop", loopFunc)

  , ("+",  numeric 0 sum)
  , ("-",  numeric 0 $ foldl1 (-))
  , ("*",  numeric 1 product)
  , ("/",  numeric 1 $ foldl1 (/))

  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))

  , ("=",  numBoolBinop (==))
  , ("!=", numBoolBinop (/=))
  , ("<",  numBoolBinop (<))
  , (">",  numBoolBinop (>))
  , ("<=", numBoolBinop (<=))
  , (">=", numBoolBinop (>=))

  , ("string=?",  strBoolBinop (==))
  , ("string!=?", strBoolBinop (/=))
  , ("string<?",  strBoolBinop (<))
  , ("string>?",  strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))

  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)

  , ("eq?",    eq)
  , ("eqv?",   eq)
  , ("equal?", eq)

  , ("if", ifMacro)
  , ("block", block)

  , ("out", out)
  , ("inL", inL)
  ]

applyFunc :: Apply
applyFunc = Function (\case
  [Atom name, List args] -> apply name args
  args                   -> throwError $ NumArgs 2 args
  )

readFunc :: Apply
readFunc = Function (const $ liftIO getLine >>= readExpr)

evalFunc :: Apply
evalFunc = Function (\case
  [val] -> eval val
  args  -> throwError $ NumArgs 1 args
  )

printFunc :: Apply
printFunc = Function (\args -> do
  liftIO $ mapM_ print args
  return $ List []
  )

loopFunc :: Apply
loopFunc = Macro (\case
  [val] -> helper val
  args  -> throwError $ NumArgs 1 args
  )
  where
    helper :: LispVal -> ThrowsError LispVal
    helper val = do
      _ <- eval val
      helper val

numeric :: Double -> ([Double] -> Double) -> Apply
numeric empty func = Function (fmap (Number . func . fill empty) . mapM toNumber)

type Converter e = LispVal -> ThrowsError e

toBool :: Converter Bool
toBool (Atom "false")   = return False
toBool (Atom "true")    = return True
toBool (Bool b)         = return b
toBool (List (b:_))     = toBool b
toBool (Number 0.0)     = return False
toBool (Number _)       = return True
toBool (String "false") = return False
toBool (String "true")  = return True
toBool e                = throwError $ TypeError "boolean" e

toNumber :: Converter Double
toNumber (Bool False) = return 0
toNumber (Bool True)  = return 1
toNumber (List (n:_)) = toNumber n
toNumber (Number n)   = return n
toNumber s@(String n) =
  maybe (throwError $ TypeError "number" s) return $ readMaybe n
toNumber e            = throwError $ TypeError "number" e

toString :: Converter String
toString (Bool   s) = return $ show s
toString (Number s) = return $ show s
toString (String s) = return s
toString e          = throwError $ TypeError "string" e

boolBinop :: Converter a -> (a -> a -> Bool) -> Apply
boolBinop conv func = Function (\case
  [a1, a2] -> do
    b1 <- conv a1
    b2 <- conv a2
    return $ Bool $ func b1 b2
  args     -> throwError $ NumArgs 2 args
  )

boolBoolBinop :: (Bool -> Bool -> Bool) -> Apply
boolBoolBinop = boolBinop toBool

numBoolBinop :: (Double -> Double -> Bool) -> Apply
numBoolBinop = boolBinop toNumber

strBoolBinop :: (String -> String -> Bool) -> Apply
strBoolBinop = boolBinop toString

car :: Apply
car = Function (\case
  [List (h:_)] -> return h
  args         -> throwError $ NumArgs 1 args
  )

cdr :: Apply
cdr = Function (\case
  [List (_:t)] -> return $ List t
  args         -> throwError $ NumArgs 1 args
  )

cons :: Apply
cons = Function (\case
  e@[_]       -> return $ List e
  [h, List t] -> return $ List $ h:t
  args        -> throwError $ NumArgs 2 args
  )

eq :: Apply
eq = boolBinop return (==)

ifMacro :: Apply
ifMacro = Macro (\case
  [ifE, thenE, elseE] -> do
    result <- eval ifE >>= toBool
    eval $ if result then thenE else elseE
  args                -> throwError $ NumArgs 3 args
  )

block :: Apply
block = Macro (fmap last . mapM eval)

out :: Apply
out = Function (\case
  [String s] -> liftIO $ flushStr s >> return (List [])
  args       -> throwError $ NumArgs 1 args
  )

inL :: Apply
inL = Function (const $ String <$> liftIO getLine)
