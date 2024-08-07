open import Agda.Builtin.Nat using (Nat; _+_)

apply2 : (Nat → Nat → Nat) → Nat → Nat → Nat
apply2 f x y = f x y

exF : Nat → Nat → Nat
exF x y = x + y
-- exF = _+_

x y z : Nat
x = apply2 _+_ 40 2
y = apply2 exF 40 2
z = apply2 (λ x y → x + y) 40 2

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}: {} | {} | {}", module_path!(),
    x(),
    y(),
    z()
  );
}
#-}
