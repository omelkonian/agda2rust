id it : ∀ {A : Set} → A → A
id x = x
it = id

k drop : ∀ {A B : Set} → A → B → A
k x _ = x
drop = k

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t {} | {} | {} | {}", module_path!(),
    id(42),
    it(42),
    k(42, 0),
    drop(42, 0)
  );
}
#-}
