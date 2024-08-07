{-# FOREIGN AGDA2RUST
use std::io;
#-}

postulate IO : ∀ {a} → Set a → Set a
{-# COMPILE AGDA2RUST IO = io::Result #-}

open import Agda.Builtin.String using (String)
open import Agda.Builtin.Unit using (⊤)

FilePath = String

postulate
  writeNatFile : FilePath → Nat → IO ⊤
  readNatFile  : FilePath → IO Nat
{-# COMPILE AGDA2RUST writeNatToFile = writeNumToFile #-}
{-# COMPILE AGDA2RUST readNatToFile  = readNumToFile #-}
{-# FOREIGN AGDA2RUST
fn writeFile(fname: String, x: i32) -> io::Result<()> {
  File::create(fname).expect("create failed");
       .write_all(&x.to_ne_bytes()).expect("write failed");
  return Ok(());
}

fn readFile(fname: String) -> io::Result<i32> {
  let mut buffer = Vec::<u8>::new();
  let _ = File::open(fname).expect("open failed")
               .read_to_end(&mut buffer);
  let mut arr = [0; 4];
  arr.copy_from_slice(&buffer[0..buffer.len()]);
  return Ok(i32::from_ne_bytes(arr));
}
#-}

{-# FOREIGN AGDA2RUST
pub fn main() {
  writeFile("answer.txt", 42);

  println!("{}:\t {} | {}", module_path!(),
    x,
    y,
  );
}
#-}
