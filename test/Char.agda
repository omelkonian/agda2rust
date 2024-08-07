open import Agda.Builtin.Char using (Char)

c4 c2 : Char
c4 = '𝟜'
c2 = '𝟚'

-- c = primNatToChar 42

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t\t {}{}", module_path!(),
    c4(), c2()
  );
}
#-}
