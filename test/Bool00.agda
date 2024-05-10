data Bool : Set where
  false true : Bool

_∧_ : Bool → Bool → Bool
_∧_ = λ where
  true true → true
  _    _    → false

-- make sure reserved Rust names do not conflict
r#true  = true
r#false = false

testBool : Bool
testBool = r#true ∧ r#false

open import Agda.Builtin.Nat using (Nat; zero; suc)

bool2Nat : Bool → Nat
bool2Nat = λ where
  true  → 0
  false → 42

-- Issue #3
isZero : Nat → Bool
isZero zero    = true
isZero (suc _) = false

toNonZero : Nat → Nat
toNonZero x with isZero x
... | true  = 1
... | false = x

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t\t\t {} | {} | {}", module_path!(),
    bool2Nat(testBool()),
    bool2Nat(isZero(5)),
    toNonZero(42),
  );
}
#-}
