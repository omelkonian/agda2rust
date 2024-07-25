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

if_then_else_ : ∀ {A : Set} → Bool → A → A → A
if true  then t else _ = t
if false then _ else f = f

testIte : Nat
testIte = if true then 42 else 0

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t\t\t {} | {}", module_path!(),
    bool2Nat(testBool()),
    testIte(),
  );
}
#-}
