-- {-# OPTIONS --exact-split #-}
{-# FOREIGN AGDA2RUST
#[derive(Debug)]
#-}
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

-- non-exact split generates "unreachable pattern" warning
min : Nat → Nat → Nat
min zero    _       = zero
min _       zero    = zero
min (suc n) (suc m) = min n m

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t {:?}", module_path!(),
    min(Nat::zero(), Nat::zero()),
  );
}
#-}
