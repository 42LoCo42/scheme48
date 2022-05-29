{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Utils where

import Control.Monad.State  (StateT, runStateT)
import Control.Monad.Writer (WriterT, runWriterT)
import System.IO            (hFlush, stdout)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

fill :: a -> [a] -> [a]
fill a [] = [a]
fill _ as = as

showLRaw :: Show a => [a] -> String
showLRaw = unwords . map show

showL :: Show a => [a] -> String
showL as = "(" ++ showLRaw as ++ ")"

orElse :: Maybe a -> Maybe a -> Maybe a
orElse a@(Just _) _ = a
orElse Nothing a@(Just _) = a
orElse Nothing Nothing = Nothing

runStateTA :: Functor m => StateT s m a -> s -> m a
runStateTA action start = fst <$> runStateT action start

runStateTS :: Functor m => StateT s m a -> s -> m s
runStateTS action start = snd <$> runStateT action start

runWriterTA :: Functor m => WriterT w m a -> m a
runWriterTA action = fst <$> runWriterT action
