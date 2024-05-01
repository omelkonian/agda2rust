open import Agda.Builtin.Nat using (Nat; _+_)

increment : Nat → Nat
increment = _+ 1

-- NB: when the `simplifyTTerm` optimization pass is enabled, we get `1 + x0`

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}: {}", module_path!(),
    increment(41)
  );
}
#-}
