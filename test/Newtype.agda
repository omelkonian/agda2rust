{-# FOREIGN AGDA2RUST
#[derive(Debug,Clone)]
#-}
data Newtype (A : Set) : Set where
  mk : A → Newtype A

private variable A : Set

k : Newtype A → Newtype A → A
k (mk a) _ = a

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t\t {}", module_path!(),
    k(Newtype::mk(42), Newtype::mk(0)),
  );
}
#-}
