{-# OPTIONS --level-universe #-}
open import Agda.Builtin.Nat using (Nat)

open import Agda.Primitive

0ℓ : Level
0ℓ = lzero

testLevel : Level → Nat
testLevel _ = 42

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}: {}", module_path!(),
    testLevel()
  );
}
#-}
