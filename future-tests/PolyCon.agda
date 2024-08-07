{-# FOREIGN AGDA2RUST
#[derive(Debug)]
#-}
data Id : Set₁ where
  id : ∀ {A : Set} → A → Id

it : ∀ {A : Set} → A → Id
it = id

{-# FOREIGN AGDA2RUST
fn main() {
  println!("{:?}", Id::id(42));
  println!("{:?}", it(42));
}
#-}
