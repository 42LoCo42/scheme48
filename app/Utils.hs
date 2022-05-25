module Utils where

import System.IO (hFlush, stdout)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

fill :: a -> [a] -> [a]
fill a [] = [a]
fill _ as = as
