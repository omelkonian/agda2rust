open import Agda.Builtin.Int using (Int; pos; negsuc)

pos42 neg42 : Int
pos42 = pos 42
neg42 = negsuc 41

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t\t\t {} | {}", module_path!(),
    pos42(),
    neg42(),
  );
}
#-}
