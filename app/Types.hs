{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods -Wno-orphans #-}
module Types where

import Control.Monad.Except (ExceptT)
import Control.Monad.State  (StateT)
import Data.Data            (Constr, Data, DataType, dataTypeOf, toConstr)
import Data.Map.Strict      (Map)
import Text.Parsec          (ParseError)

import Utils

data LispVal
  = Atom String
  | List [LispVal]
  | Number Double
  | String String
  | Bool Bool
  deriving (Eq, Data)

lispValT :: DataType
lispValT = dataTypeOf $ Atom ""

atomC :: Constr
atomC = toConstr $ Atom ""

listC :: Constr
listC = toConstr $ List []

numberC :: Constr
numberC = toConstr $ Number 0

stringC :: Constr
stringC = toConstr $ String ""

boolC :: Constr
boolC = toConstr $ Bool False

pattern QuoteP :: LispVal
pattern QuoteP = Atom "quote"

pattern QuasiquoteP :: LispVal
pattern QuasiquoteP = Atom "quasiquote"

pattern UnquoteP :: LispVal
pattern UnquoteP = Atom "unquote"

showQuote :: LispVal -> String
showQuote QuoteP      = "'"
showQuote QuasiquoteP = "`"
showQuote UnquoteP    = ","
showQuote v           = error "Not a quote: " ++ show v

isQuote :: LispVal -> Bool
isQuote = flip elem [QuoteP, QuasiquoteP, UnquoteP]

instance Show LispVal where
  show (String contents)    = "\"" ++ contents ++ "\""
  show (Atom name)          = name
  show (Number contents)    = show contents
  show (Bool True)          = "#t"
  show (Bool False)         = "#f"
  show (List [])            = "()"
  show (List l@(h:t))
    | isQuote h             = showQuote h ++ showLRaw t
    | otherwise             = showL l

data LispError
  = BadForm LispVal
  | NoFunction String
  | NoType     String
  | NoVariable String
  | NumArgs [Param] [LispVal]
  | ParseErr ParseError
  | TypeError String LispVal
  | Unquote LispVal

instance Show LispError where
  show (BadForm     val) = "Unrecognized form: " ++ show val
  show (NoFunction  val) = "No such function: " ++ val
  show (NoType      val) = "No such type: " ++ val
  show (NoVariable  val) = "No such variable: " ++ val
  show (NumArgs  n vals) = "Need " ++ showL n ++ ", but got " ++ show (List vals)
  show (ParseErr    err) = "Parser error: " ++ show err
  show (TypeError n val) = "Could not convert to " ++ n ++ ": " ++ show val
  show (Unquote     val) = "unquote not within quasiquote: " ++ show val

data Param
  = All  { argName :: String }
  | Rest { argName :: String }
  | Any  { argName :: String }
  | PList [Param]
  | Var  { argName :: String, constr :: Constr }
  deriving (Eq)

instance Show Param where
  show (All   name)     = ":all " ++ name
  show (Rest  name)     = ":rest " ++ name
  show (Any   name)     = ":any " ++ name
  show (PList params)   = showL params
  show (Var   name typ) = show typ ++ " " ++ name

data Function = Function
  { closure    :: Env
  , parameters :: [Param]
  , evalAll    :: Bool
  , body       :: [LispVal] -> Lisp
  }

instance Show Function where
  show f = "(" ++
    if evalAll f then "defun" else "defmacro"
    ++ " " ++ showL (parameters f)
    ++ ")"

type Vars = Map String LispVal
type Defs = Map String Function
type Env  = (Vars, Defs)
newtype GlobalEnv = G { global :: Env }
newtype LocalEnv  = L { local  :: Env }

type E    = ExceptT LispError IO
type EEnv = StateT LocalEnv (StateT GlobalEnv E)
type Lisp = EEnv LispVal
