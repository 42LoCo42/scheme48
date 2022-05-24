module Read where

import Text.ParserCombinators.Parsec hiding (spaces)

import Error
import Lisp

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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
  elems <- sepBy parseExpr spaces
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
parseExpr = parseString
        <|> try parseNumber
        <|> parseAtom
        <|> parseQuoted
        <|> parseList

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> throwError $ ParseErr err
  Right val -> return val

