module Postulates where

open import Agda.Builtin.Nat using (Nat; zero; suc)

postulate TODO : ∀ {A : Set} → A

max : Nat → Nat
max = TODO

testMax : Nat
testMax with 0
... | zero  = 42
... | suc _ = max 42

-- postulate Key : Set

-- idKey : Key → Key
-- idKey k = k

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}: {}", module_path!(),
    testMax(),
  );
}
#-}
