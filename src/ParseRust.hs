module Main where

import System.Environment ( getArgs )
import Control.Monad ( void )

import Rust.Lib ( SourceFile, Span, parse', readInputStream, printRust )

main :: IO ()
main = do
  [rustFn] <- getArgs
  src <- void . parse' @(SourceFile Span) <$> readInputStream rustFn
  printRust src
