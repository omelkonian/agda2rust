{-# LANGUAGE TypeApplications #-}
module Main where

import System.Environment ( getArgs )
import Control.Monad ( void )
import Text.Show.Pretty ( pPrint )

import Rust.Lib ( SourceFile, Span, parse', readInputStream, pretty' )

main :: IO ()
main = do
  [rustFn] <- getArgs
  src <- void . parse' @(SourceFile Span) <$> readInputStream rustFn
  let hline = putStrLn "----------------------"
  hline >> print (pretty' src) >> hline >> pPrint src >> hline
