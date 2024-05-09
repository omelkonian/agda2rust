open import Agda.Builtin.Nat using (Nat)

constNat : Nat → Nat → Nat
constNat x _ = x

the42 : Nat
the42 = constNat 42 0

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t {} | {}", module_path!(),
    constNat(42, 41),
    the42(),
  );
}
#-}
