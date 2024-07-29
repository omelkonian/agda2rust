-- Issue #2
open import Agda.Builtin.Nat using (suc) renaming (Nat to ℕ)

{-# FOREIGN AGDA2RUST
#[derive(Debug)]
#-}
data Foo : Set where
  foo : ℕ → Foo

f : ℕ → Foo
f = foo

g : ℕ → ℕ
g = suc

h : ℕ → ℕ
h = g

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t {:?} | {} | {}", module_path!(),
    f(42),
    g(41),
    h(41),
  );
}
#-}
