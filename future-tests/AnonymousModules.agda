module _ where

open import Agda.Builtin.Nat using (Nat)

module _ {ℓ} (A : Set ℓ) where
  data BarA : Set ℓ where
    bar : A → BarA

  data FooA : Set ℓ where
    foo : {BarA} → FooA

fooAToNat : FooA Nat → Nat
fooAToNat (foo {bar n}) = n

testFooA : FooA Nat
testFooA = foo {_}{_}{bar 42}

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t {} | {}", module_path!(),
    fooToNat(testFoo()),
    fooAToNat(testFooA()),
  );
}
#-}
