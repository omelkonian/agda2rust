module _ where

open import Agda.Builtin.Nat using (Nat; suc; _+_)

module M (n : Nat) where
  add40 add41 : Nat
  add40 = 40 + n
  add41 = suc add40

open M 1

testAdd40 testAdd41 : Nat
testAdd40 = suc add40
testAdd41 = add41

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t {} | {} | {} | {}", module_path!(),
    add40(2),
    add41(1),
    testAdd40(),
    testAdd41(),
  );
}
#-}
