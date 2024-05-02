open import Agda.Builtin.String using (String)

s42 : String
s42 = "42"

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t {}", module_path!(),
    s42()
  );
}
#-}
