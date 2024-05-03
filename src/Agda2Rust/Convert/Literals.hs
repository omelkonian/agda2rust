module Agda2Rust.Convert.Literals () where

import Data.Text ( unpack )
import Data.Serializer ( toBytes )

import Utils
import qualified Agda as A
import Agda ( panic )
import qualified Rust as R
import Agda2Rust.Convert.Class

-- | Compiling literals.
instance A.Literal ~> R.Lit where
  go = return . \case
    A.LitNat    i -> R.Int R.Dec i R.Unsuffixed ()
    A.LitWord64 w -> R.ByteStr (toBytes w) R.Cooked R.Unsuffixed ()
    A.LitFloat  d -> R.Float d R.Unsuffixed ()
    A.LitString s -> R.Str (unpack s) R.Cooked R.Unsuffixed ()
    A.LitChar   c -> R.Char c R.Unsuffixed ()
    l             -> panic "literal" l
