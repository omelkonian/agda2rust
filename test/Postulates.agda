open import Agda.Builtin.Nat using (Nat; zero; suc)

postulate TODO : ∀ {A : Set} → A
{-# COMPILE AGDA2RUST TODO const #-}

max : Nat → Nat
max = TODO
{-# COMPILE AGDA2RUST max const #-}

testMax : Nat
testMax with 0
... | zero  = 42
... | suc _ = max 42
{-# COMPILE AGDA2RUST testMax const #-}

getTestMax : Nat
getTestMax = testMax

postulate Key : Set
{-# COMPILE AGDA2RUST Key = u64 #-}

idKey : Key → Key
idKey k = k

postulate getDefaultKey : Key
{-# COMPILE AGDA2RUST getDefaultKey = getDefaultKey #-}
{-# FOREIGN AGDA2RUST
fn getDefaultKey() -> u64 {
  42
}
#-}

testGetKey : Key
testGetKey = getDefaultKey

postulate defaultKey : Key
{-# COMPILE AGDA2RUST defaultKey const = 42 #-}

testKey : Key
testKey = defaultKey
{-# COMPILE AGDA2RUST testKey const #-}

postulate hash : ∀ {A : Set} → A → Key
{-# COMPILE AGDA2RUST hash = idHash #-}
{-# FOREIGN AGDA2RUST
use std::hash::{DefaultHasher, Hash, Hasher};

fn idHash<A: Hash>(x: i32) -> u64 {
  let mut s = DefaultHasher::new();
  x.hash(&mut s);
  s.finish()
}
#-}

testHash : Key
testHash = hash testMax

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t {} | {} | {} | {} | {} | hash(42)={}", module_path!(),
    testMax,
    getTestMax(),
    idKey(42),
    testGetKey(),
    testKey,
    testHash(),
  );
}
#-}
