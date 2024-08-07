-- const translates to `r#const`
-- TODO: properly handle these Rust keywords in expressions

const : ∀ {A B : Set} → A → B → A
const x _ = x

{-# FOREIGN AGDA2RUST
fn main() {
  println!("{}", const(42, 0));
}
#-}

open import Agda.Builtin.Nat using (Nat)

i32 = Nat
{-# COMPILE AGDA2RUST i32 ignore #-}

the42 : i32
the42 = const 42 0

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}: {} | {}", module_path!(),
    r#const(42, 41),
    the42(),
  );
}
#-}
