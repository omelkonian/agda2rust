{-

Demonstrates bug in `language-rust` package, not fixed by any fork.

* Introduces a redundant `+` bound on `impl` traits,
  which causes an error when it appears on the return type.

* Ad-hoc fix for now in `src/Main.hs::fixCode`.

-}

Id0000000000000000000000000000000000000000000000000000000000000000 : Set → Set
Id0000000000000000000000000000000000000000000000000000000000000000 A = A

id : let Id = Id0000000000000000000000000000000000000000000000000000000000000000 in
  ∀ {A : Set} → (Id A -> A) → A → A
id _ x = x

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t {}", module_path!(),
    id(|x| x, 42),
  );
}
#-}
