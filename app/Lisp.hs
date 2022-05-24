{-# LANGUAGE PatternSynonyms #-}
module Lisp where

data LispVal
  = Atom String
  | List [LispVal]
  | Number Double
  | String String
  | Bool Bool
  deriving (Eq)

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
  show (String contents)   = "\"" ++ contents ++ "\""
  show (Atom name)         = name
  show (Number contents)   = show contents
  show (Bool True)         = "#t"
  show (Bool False)        = "#f"
  show (List [])           = "()"
  show (List l@(h:t))
    | isQuote h            = showQuote h ++ unwords (map show t)
    | otherwise            = "(" ++ unwords (map show l) ++ ")"
