open import Agda.Builtin.Bool using (Bool; true; false)

tt = true

not : Bool → Bool
not true  = false
not false = true

_∧_ : Bool → Bool → Bool
_∧_ = λ where
  true true → true
  _    _    → false

testBool : Bool
testBool = true ∧ false

open import Agda.Builtin.Nat using (Nat)

bool2Nat : Bool → Nat
bool2Nat = λ where
  true  → 0
  false → 42

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t {}", module_path!(),
    bool2Nat(testBool())
  );
}
#-}
