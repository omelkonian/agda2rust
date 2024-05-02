postulate Float : Set
{-# BUILTIN FLOAT Float #-}

idFloat : Float → Float
idFloat f = f

testFloat : Float
testFloat = idFloat 4.2

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}: {}", module_path!(),
    idFloat(testFloat())
  );
}
#-}
