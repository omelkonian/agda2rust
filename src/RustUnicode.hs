module Main where

import System.Environment ( getArgs )
import Data.Char ( chr )

main :: IO ()
main = do
  fn:as <- getArgs
  let outFn = case as of [] -> fn; (fn:_) -> fn
  putStrLn $ "Running reUnicode on: " <> fn
  s <- readFile fn
  putStrLn $ "Before:\n" <> s
  let s' = reUnicode s
  putStrLn $ "After:\n" <> s'
  writeFile outFn s'

reUnicode :: String -> String
reUnicode = go
  where
  nextMark = break (== 'Ֆ')

  go s
    | (_, "") <- nextMark s
    = s
    | (s0, 'Ֆ':s) <- nextMark s
    , (_, "")     <- nextMark s
    = s
    | (s0, 'Ֆ':s) <- nextMark s
    , (sn, 'Ֆ':s) <- nextMark s
    = s0 <> [chr $ read sn] <> go s
    | otherwise
    = error $ "reUnicode: impossible case <" <> show s <> ">"

