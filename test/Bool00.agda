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

open import Agda.Builtin.Nat using (Nat)

bool2Nat : Bool → Nat
bool2Nat = λ where
  true  → 0
  false → 42

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t\t\t {:?}", module_path!(),
    bool2Nat(testBool())
  );
}
#-}
