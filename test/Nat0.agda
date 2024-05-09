{-# FOREIGN AGDA2RUST #[derive(Debug)] #-}
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

_+_ : Nat → Nat → Nat
zero  + m = m
suc n + m = suc (n + m)

{-# FOREIGN AGDA2RUST
fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

use self::Nat::{zero,suc};

pub fn main() {
  println!("{}:\t\t\t {:?} | {:?}", module_path!(),
    suc(ᐁ(suc(ᐁ(suc(ᐁ(suc(ᐁ(zero())))))))),
    _Ֆ43Ֆ_(suc(ᐁ(zero())), suc(ᐁ(zero()))),
  );
}
#-}
