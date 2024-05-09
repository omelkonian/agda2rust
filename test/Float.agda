open import Agda.Builtin.Float using (Float)

idFloat : Float → Float
idFloat f = f

testFloat : Float
testFloat = idFloat 4.2

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t\t\t {}", module_path!(),
    idFloat(testFloat())
  );
}
#-}
