{-# FOREIGN AGDA2RUST #[derive(Debug)] #-}
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

_+_ : Nat → Nat → Nat
zero  + m = m
suc n + m = suc (n + m)
