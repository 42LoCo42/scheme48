module Read where

import Control.Monad.Except          (throwError)
import Text.ParserCombinators.Parsec hiding (spaces)

import Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

comment :: Parser Char
comment = char ';' >> many (noneOf "\n") >> char '\n'

spaces :: Parser ()
spaces = skipMany1 (comment <|> oneOf " \t\n")

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> symbol <|> digit)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  sign <- option "" $ (:[]) <$> char '-'
  whole <- many1 digit
  fract <- option "" $ do
    dot <- char '.'
    digits <- many1 digit
    return $ dot : digits
  return $ Number $ read $ sign ++ whole ++ fract

parseList :: Parser LispVal
parseList = do
  _ <- char '('
  elems <- optional spaces >> sepEndBy parseExpr spaces
  _ <- char ')'
  return $ List elems

parseQuoted :: Parser LispVal
parseQuoted = do
  quoteChar <- oneOf "'`,"
  x <- parseExpr
  let
    atom = case quoteChar of
      '\'' -> QuoteP
      '`'  -> QuasiquoteP
      ','  -> UnquoteP
      _    -> error "UNREACHABLE"
  return $ List [atom, x]

parseExpr :: Parser LispVal
parseExpr = do
  optional spaces
  parseString
    <|> try parseNumber
    <|> parseAtom
    <|> parseQuoted
    <|> parseList

readExpr :: String -> Lisp
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> throwError $ ParseErr err
  Right val -> return val

