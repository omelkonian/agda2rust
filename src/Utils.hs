-- | Language-independent utilities.
module Utils where

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Unicode.Char.Identifiers ( isXIDStart, isXIDContinue )
import Data.Char ( ord )

-- ** basics
enumerate0, enumerate :: [a] -> [(Int, a)]
enumerate0 = zip [0..]
enumerate  = zip [1..]

(/\), (\/) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(p /\ q) x = p x && q x
(p \/ q) x = p x || q x

-- ** printing
report :: MonadIO m => String -> m ()
report s = liftIO $ putStrLn s

-- | Translate a Unicode string to Rust-compatible identifier.
--
-- c.f. https://doc.rust-lang.org/reference/identifiers.html
transcribe :: String -> String
transcribe ""     = error "[UNEXPECTED] empty identifier"
transcribe "_"    = error "[UNEXPECTED] placeholder identifier `_` should not be compiled"
transcribe (c:cs) = toXIDStart c <> concatMap toXIDContinue cs
  where
  -- TODO: malicious user can break compiler by reverse engineering an identifier
  -- * SOLUTION: keep a state of all produced identifiers and generate fresh ones
  toXIDStart, toXIDContinue :: Char -> String
  toXIDStart c
    | isXIDStart c || c == '_'
    = [c]
    | isXIDContinue c
    = "_" <> [c]
    | otherwise
    = toXIDContinue c

  toXIDContinue c
    | isXIDContinue c
    = [c]
    | otherwise
    = "Ֆ" <> show (ord c) <> "Ֆ"

-- ** bitmasks

type Bitmask  = [Bool]

select :: Bitmask -> [a] -> [a]
select (b:bs) (x:xs) = [x|b] <> select bs xs
select []     []     = []
select _      _      = error "select: bitmask of different length"
