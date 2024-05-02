data Bool : Set where
  false true : Bool
{-# BUILTIN BOOL  Bool  #-}
{-# BUILTIN FALSE false #-}
{-# BUILTIN TRUE  true  #-}

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

data Nat : Set where
  zero : Nat
  suc  : (n : Nat) → Nat
{-# BUILTIN NATURAL Nat #-}

bool2Nat : Bool → Nat
bool2Nat = λ where
  true  → 0
  false → 42

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}: {}", module_path!(),
    bool2Nat(testBool())
  );
}
#-}
