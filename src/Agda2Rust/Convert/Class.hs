module Agda2Rust.Convert.Class where

import Utils ( enumerate0 )
import Agda2Rust.Monad ( C, inArgument )

type (:~>)  a b = a -> C (b ())
type (:~>*) a b = a -> C [b ()]

-- | Converting between two types @a@ and @b@ under Agda2Rust's monad.
--
-- NB: 'go' is only used internally to de-clutter the recursive calls.
class (~>) a b | a -> b where
  convert, go :: a :~> b
  convert = go

-- | Iteratively convert a list of arguments (keeping track of their index).
gos :: forall a b. a ~> b => [a] :~>* b
gos = igos . enumerate0
  where
  igos :: [(Int, a)] :~>* b
  igos [] = return []
  igos ((i, a):as) = (:) <$> inArgument i (go a) <*> igos as

