module Constants where

open import Agda.Builtin.Nat using (Nat)

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

private variable A : Set

naught : Maybe A
naught = nothing

justify : Maybe A → A → A
justify nothing  x = x
justify (just x) _ = x

testNaught : Nat
testNaught = justify (naught {A = Nat}) 42
-- {-# COMPILE AGDA2RUST testNaught const #-}
-- NB: cannot declare this as const or static due to Rust limitations

the42 : Nat
the42 = 42
{-# COMPILE AGDA2RUST the42 const #-}

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t {} | {}", module_path!(),
    testNaught(),
    the42
  );
}
#-}
