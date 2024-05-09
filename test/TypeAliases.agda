module TypeAliases where

open import Agda.Builtin.Nat using (Nat; _+_)

ℕ : Set
ℕ = Nat

testAlias : ℕ
testAlias = 42

ℕ→ℕ : Set
ℕ→ℕ = ℕ → ℕ

incr : ℕ→ℕ
incr = _+ 1

testAliasF : ℕ
testAliasF = incr 41

Id : Set → Set
Id A = A

id : ∀ {A : Set} → Id A → Id A
id x = x

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t\t {} | {} | {}", module_path!(),
    testAlias(),
    testAliasF(),
    id(42),
  );
}
#-}
