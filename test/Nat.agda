open import Agda.Builtin.Nat using (Nat; zero; suc)

_+_ : Nat → Nat → Nat
zero  + m = m
suc n + m = suc (n + m)

testNat : Nat
testNat = 40 + 2

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t\t\t {}", module_path!(),
    testNat()
  );
}
#-}
