{-# LANGUAGE TypeApplications #-}
module Main where

import System.Environment ( getArgs )
import Control.Monad ( void )
import Text.Show.Pretty ( pPrint, ppDoc )

import Language.Rust.Syntax ( SourceFile )
import Language.Rust.Data.Position ( Span )
import Language.Rust.Parser ( parse', readInputStream )
import Language.Rust.Pretty ( Doc, pretty' )

main :: IO ()
main = do
  [rustFn] <- getArgs
  src <- void . parse' @(SourceFile Span) <$> readInputStream rustFn
  let hline = putStrLn "----------------------"
  hline >> print (pretty' src) >> hline >> print (ppDoc src) >> hline
