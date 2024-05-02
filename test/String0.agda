postulate String : Set
{-# BUILTIN STRING String #-}

s42 : String
s42 = "42"

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}: {}", module_path!(),
    s42()
  );
}
#-}
