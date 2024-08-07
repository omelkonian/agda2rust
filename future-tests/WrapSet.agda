{-# FOREIGN AGDA2RUST
#[derive(Debug)]
#-}
data WrapSet : Set₁ where
  wrap : Set → WrapSet

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t {}", module_path!(),
    WrapSet::wrap()
  );
}
#-}
