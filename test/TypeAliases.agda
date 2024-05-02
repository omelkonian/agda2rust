module TypeAliases where

open import Agda.Builtin.Nat using (Nat; _+_)

ℕ : Set
ℕ = Nat

-- below, ℕ gets reduced to Nat
testAlias : ℕ
testAlias = 42

ℕ→ℕ : Set
ℕ→ℕ = ℕ → ℕ

incr : ℕ→ℕ
incr = _+ 1

testAliasF : ℕ
testAliasF = incr 41

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}: {} | {}", module_path!(),
    testAlias(),
    testAliasF(),
  );
}
#-}
