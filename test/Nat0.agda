{-# FOREIGN AGDA2RUST
#[derive(Debug)]
#-}
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

_+_ : Nat → Nat → Nat
zero  + m = m
suc n + m = suc (n + m)

{-# FOREIGN AGDA2RUST
use self::Nat::{zero,suc};

pub fn main() {
  println!("{}:\t\t {:?} | {:?}", module_path!(),
    suc(ᐁ(suc(ᐁ(suc(ᐁ(suc(ᐁ(zero())))))))),
    _Ֆ43Ֆ_(suc(ᐁ(zero())), suc(ᐁ(zero()))),
  );
}
#-}
