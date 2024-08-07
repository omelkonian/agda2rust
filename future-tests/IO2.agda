open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Unit using (⊤)

{-# FOREIGN AGDA2RUST
mod std::io::prelude::*;
#-}

postulate
  FileHandle : Set
  stdout     : FileHandle
  hPutStrLn  : FileHandle → String → IO ⊤
{-# COMPILE AGDA2RUST FileHandle = type std::io::File #-}
{-# COMPILE AGDA2RUST stdout     = std::io::stdout #-}
{-# COMPILE AGDA2RUST hPutStrLn  = std::io::putStrLn #-}


{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t {} | {}", module_path!(),
    test??(),
    test??(),
  );
}
#-}
