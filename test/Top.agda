{-# FOREIGN AGDA2RUST
#[derive(Debug)]
#-}
data Unit : Set where
  tt : Unit

idUnit : Unit → Unit
idUnit tt = tt

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t\t\t {:?}", module_path!(),
    idUnit(Unit::tt())
  );
}
#-}
