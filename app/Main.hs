module Main where

import Control.Monad.Except
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Read                     (readMaybe)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | Number Double
  | String String
  | Bool Bool
  deriving (Eq)

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
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

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

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name)       = name
  show (Number contents) = show contents
  show (Bool True)       = "#t"
  show (Bool False)      = "#f"
  show (List contents)   = "(" ++ unwords (map show contents) ++ ")"

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Bool   _)             = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm                    = throwError $ BadForm badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
  (throwError $ NoFunction func)
  ($ args) $ lookup func primitives

foldingBool :: (a -> a -> Bool) -> Bool -> [a] -> Bool
foldingBool _ start [] = start
foldingBool func start as@(a:_) = foldl

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numeric 0 sum)
  , ("-", numeric 0 $ foldl1 (-))
  , ("*", numeric 1 product)
  , ("/", numeric 1 $ foldl1 (/))
  , ("<", numeric 0 $ foldl1 (<))
  , ("=", \args -> return $ Bool $ case args of
      [] -> True
      (h:t) -> all (h == ) t
    )
  ]

fill :: a -> [a] -> [a]
fill a [] = [a]
fill _ as = as

numeric :: Double -> ([Double] -> Double) -> [LispVal] -> ThrowsError LispVal
numeric empty func args = Number . func . fill empty <$> mapM getNumber args
  where
    getNumber :: LispVal -> ThrowsError Double
    getNumber (Number n)   = return n
    getNumber s@(String n) =
      maybe (throwError $ NoNumber s) return $ readMaybe n
    getNumber (List (n:_)) = getNumber n
    getNumber (Bool True)  = return 1
    getNumber (Bool False) = return 0
    getNumber e            = throwError $ NoNumber e

boolean :: Bool -> ([Bool] -> Bool) -> [LispVal] -> ThrowsError LispVal
boolean empty func args = Bool . func . fill empty <$> mapM getBool args
  where
    getBool :: LispVal -> ThrowsError Bool
    getBool (Bool b)         = return b
    getBool (Atom "true")    = return True
    getBool (Atom "false")   = return False
    getBool (String "true")  = return True
    getBool (String "false") = return False
    getBool (List (b:_))     = getBool b
    getBool (Number 0.0)     = return False
    getBool (Number _)       = return True
    getBool e                = throwError $ NoBool e

data LispError
  = BadForm LispVal
  | NoBool LispVal
  | NoFunction String
  | NoNumber LispVal
  | NumArgs Int LispVal
  | ParseErr ParseError

instance Show LispError where
  show (BadForm    val) = "Unrecognized form: " ++ show val
  show (NoBool     val) = "Could not extract boolean from " ++ show val
  show (NoFunction val) = "No such function: " ++ val
  show (NoNumber   val) = "Could not extract number from " ++ show val
  show (NumArgs n  val) = "Need " ++ show n ++ " arguments, but got " ++ show val
  show (ParseErr   err) = "Parser error: " ++ show err

type ThrowsError = Either LispError

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ either
    (\e -> "ERROR: " ++ show e)
    show
    (readExpr expr >>= eval)
