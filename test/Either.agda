open import Agda.Builtin.Nat using (Nat; _+_)

data Either (A B : Set) : Set where
  Left  : A → Either A B
  Right : B → Either A B

fromEither : ∀ {A : Set} → A → ∀ {B : Set} → Either A B → A
fromEither _    (Left  a) = a
fromEither defA (Right _) = defA

data OnlyLeft (A B : Set) : Set where
  Left : A → OnlyLeft A B

fromOnlyLeft : ∀ {A B : Set} → OnlyLeft A B → A
fromOnlyLeft (Left a) = a

record OnlyLeftR (A B : Set) : Set where
  field left : A
open OnlyLeftR public

fromOnlyLeftR : ∀ {A B : Set} → OnlyLeftR A B → A
fromOnlyLeftR r = r .left

{-# FOREIGN AGDA2RUST
use std::marker::{PhantomData};
fn __<T>() -> PhantomData<T> { return PhantomData; }

pub fn main() {
  println!("{}:\t {} | {} | {}", module_path!(),
    fromEither::<i32, i32>(41, Either::Left(42)),
    fromOnlyLeft::<i32, i32>(OnlyLeft::Left(42)),
    fromOnlyLeftR::<i32, i32>(OnlyLeftR {left: 42, _phantom: __()}),
  );
}
#-}
