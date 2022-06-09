{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Apply where

import Control.Monad        (foldM, when)
import Control.Monad.Except (catchError, liftIO, throwError)
import Control.Monad.State  (StateT, get, gets, lift, modify, put)
import Data.Data            (readConstr, toConstr)
import Data.Map.Strict      (Map, empty, fromList, insert, union, (!), (!?))
import Data.Maybe           (fromJust)
import Text.Read            (readMaybe)

import {-# SOURCE #-} Eval  (eval)
import                Read
import                Types
import                Utils

clean :: Env
clean = (empty, primitives)

bindArgs :: [Param] -> [LispVal] -> E Vars
bindArgs expected got =
  runStateTS (runStateTS (mapM_ bindArg expected) got) empty
  where
    err :: StateT [LispVal] (StateT Vars E) a
    err = lift $ throwError $ NumArgs expected got

    bindArg :: Param -> StateT [LispVal] (StateT Vars E) ()
    bindArg (All   name)     = lift $ bind name (List got)
    bindArg (Rest  name)     = get >>= (lift . bind name . List)
    bindArg (Any   name)     = consume >>= (lift . bind name)
    bindArg (PList args)     = consume >>= (\case
      List list -> lift (lift $ bindArgs args list) >>= (lift . bindAll)
      _         -> err
      )
    bindArg (Var   name typ) = consume >>= (\val ->
      if toConstr val == typ
      then lift $ bind name val
      else err
      )

    consume :: StateT [LispVal] (StateT Vars E) LispVal
    consume = do
      args <- get
      when (null args) err
      let ret = head args
      put $ tail args
      return ret

    bind :: String -> LispVal -> StateT Vars E ()
    bind var val = modify (insert var val)

    bindAll :: Vars -> StateT Vars E ()
    bindAll new = modify (union new)

argStructure :: Vars -> [Param] -> [LispVal]
argStructure vars = map helper
  where
    helper (PList list) = List $ argStructure vars list
    helper  arg         = vars ! argName arg

paramStructure :: [LispVal] -> E [Param]
paramStructure [] = return []
paramStructure (Atom typ : Atom name : rest) = do
  this <- case typ of
    ":all"  -> return $ All name
    ":rest" -> return $ Rest name
    ":any"  -> return $ Any name
    _       -> maybe
      (throwError $ NoType typ)
      (return . Var name)
      (readConstr lispValT typ)
  (this :) <$> paramStructure rest

paramStructure (List list : rest) = do
  this <- PList <$> paramStructure list
  (this :) <$> paramStructure rest

paramStructure vals = throwError $ BadForm $ List vals

apply :: String -> [LispVal] -> Lisp
apply name args = do
  gDefs <- lift $ gets (snd . global)
  (lVars, lDefs) <- gets local
  case gDefs !? name of
    Nothing     -> throwError $ NoFunction name
    (Just func) -> do
      let params = parameters func
      let (cloVars, cloDefs) = closure func

      boundVars <-
        mapM (if evalAll func then eval else return) args >>=
        (lift . lift . bindArgs params)

      let
        runEnv =
          ( boundVars `union` cloVars `union` lVars
          , cloDefs `union` lDefs
          )

      lift $ runStateTA
        (body func $ argStructure boundVars params) (L runEnv)

primitives :: Map String Function
primitives = fromList
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

  , ("id", Function clean [Any "val"] True (return . head))

  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("map", mapFunc)
  , ("fold", foldFunc)

  , ("eq?",    eq)
  , ("eqv?",   eq)
  , ("equal?", eq)

  , ("if", ifMacro)
  , ("block", block)

  , ("out", out)
  , ("outL", outL)
  , ("inL", inL)

  , ("try", try)

  , ("define", define)
  , ("let", letFunc)

  , ("defun", defun)
  , ("lambda", lambda)
  ]

applyFunc :: Function
applyFunc = Function clean [Var "function" atomC, Var "args" listC] True
  (\[Atom name, List args] -> apply name args)

readFunc :: Function
readFunc = Function clean [] True (const $ liftIO getLine >>= readExpr)

evalFunc :: Function
evalFunc = Function clean [Any "expr"] True (\[val] -> eval val)

printFunc :: Function
printFunc = Function clean [All "args"] True (\[List args] -> do
  liftIO $ mapM_ print args
  return $ List []
  )

loopFunc :: Function
loopFunc = Function clean [Any "expr"] False (\[val] -> helper val)
  where
    helper :: LispVal -> Lisp
    helper val = do
      _ <- eval val
      helper val

numeric :: Double -> ([Double] -> Double) -> Function
numeric start func = Function clean [All "args"] True
  (\[List args] -> Number . func . fill start <$> mapM toNumber args)

type Converter e = LispVal -> EEnv e

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

boolBinop :: Converter a -> (a -> a -> Bool) -> Function
boolBinop conv func = Function clean [Any "b1", Any "b2"] True
  (\[a1, a2] -> do
    b1 <- conv a1
    b2 <- conv a2
    return $ Bool $ func b1 b2
  )

boolBoolBinop :: (Bool -> Bool -> Bool) -> Function
boolBoolBinop = boolBinop toBool

numBoolBinop :: (Double -> Double -> Bool) -> Function
numBoolBinop = boolBinop toNumber

strBoolBinop :: (String -> String -> Bool) -> Function
strBoolBinop = boolBinop toString

car :: Function
car = Function clean [Var "list" listC] True (\[List (h:_)] -> return h)

cdr :: Function
cdr = Function clean [Var "list" listC] True (\[List (_:t)] -> return $ List t)

cons :: Function
cons = Function clean [Any "val", Var "list" listC] True
  (\[h, List t] -> return $ List $ h:t)

mapFunc :: Function
mapFunc = Function clean [Var "func" atomC, Var "list" listC] True
  (\[Atom func, List l] -> List <$> mapM (apply func . (:[])) l)

foldFunc :: Function
foldFunc = Function clean [Var "func" atomC, Var "list" listC] True
  (\[Atom func, List l] -> case l of
    []   -> return $ List []
    vals -> foldM (\accum val ->
      apply func [accum, val]) (head vals) (tail vals)
  )

eq :: Function
eq = boolBinop return (==)

ifMacro :: Function
ifMacro = Function clean [Any "if", Any "then", Any "else"] False
  (\[ifE, thenE, elseE] -> do
    result <- eval ifE >>= toBool
    eval $ if result then thenE else elseE
  )

block :: Function
block = Function clean [All "exprs"] False
  (\[List args] -> last <$> mapM eval args)

out :: Function
out = Function clean [Var "string" stringC] True
  (\[String s] -> liftIO $ flushStr s >> return (List []))

outL :: Function
outL = Function clean [Var "string" stringC] False
  (\[String s] -> liftIO $ flushStr (s ++ "\n") >> return (List []))

inL :: Function
inL = Function clean [] True (const $ String <$> liftIO getLine)

try :: Function
try = Function clean [Any "expr", Var "handler" atomC] False
  (\[expr, Atom handler] ->
    eval expr `catchError` (apply handler . (: []) . String . show)
  )

define :: Function
define = Function clean [Var "var" atomC, Any "val"] False
  (\[Atom var, val] -> do
    evaled <- eval val
    lift $ modify (\(G (vars, defs)) -> G (insert var evaled vars, defs))
    return evaled
  )

letFunc :: Function
letFunc = Function clean [Var "defs" listC, Rest "exprs"] False
  (\[List defs, List exprs] -> do
    boundDefs <- fromList <$> mapM (\def -> do
      boundDef <- lift $ lift $
        bindArgs [PList [Var "var" atomC, Any "val"]] [def]
      let (Atom var) = fromJust $ boundDef !? "var"
      val <- eval $ fromJust $ boundDef !? "val"
      return (var, val)
      ) defs
    modify (\(L (vars, defs')) -> L (boundDefs `union` vars, defs'))
    last <$> mapM eval exprs
  )

mkFunction :: [LispVal] -> [LispVal] -> EEnv Function
mkFunction params exprs = do
  env <- gets local
  structure <- lift $ lift $ paramStructure params
  return $ Function env structure True (const $ last <$> mapM eval exprs)

defun :: Function
defun = Function clean
  [Var "name" atomC, Var "params" listC, Rest "exprs"] False
  (\[Atom name, List params, List exprs] -> do
    func <- mkFunction params exprs
    lift $ modify (\(G (vars, defs)) -> G (vars, insert name func defs))
    return $ List []
  )

lambda :: Function
lambda = Function clean
  [Var "params" listC, Rest "exprs"] False
  (\[List params, List exprs] -> do
    let name = "'lambda"
    func <- mkFunction params exprs
    modify (\(L (vars, defs)) -> L (vars, insert name func defs))
    return $ Atom name
  )
