open import Agda.Builtin.Nat using (Nat; _+_)

-- ** no unused type parameters
data Either (A B : Set) : Set where
  Left  : A → Either A B
  Right : B → Either A B

fromEither : ∀ {A : Set} → A → ∀ {B : Set} → Either A B → A
fromEither _    (Left  a) = a
fromEither defA (Right _) = defA

-- ** unused type parameters present
data OnlyLeft (A B : Set) : Set where
  Left : A → OnlyLeft A B

private variable A B C : Set

fromOnlyLeft : OnlyLeft A B → A
fromOnlyLeft (Left a) = a

fromOnlyLeft2 : OnlyLeft (OnlyLeft A B) C → A
fromOnlyLeft2 (Left (Left a)) = a

record OnlyLeftR (A B : Set) : Set where
  field left : A
open OnlyLeftR public

fromOnlyLeftR : OnlyLeftR A B → A
fromOnlyLeftR r = r .left

fromOnlyLeftR2 : OnlyLeftR (OnlyLeftR A B) C → A
fromOnlyLeftR2 r = r .left .left

{-# FOREIGN AGDA2RUST
use std::marker::{PhantomData};
fn __<T>() -> PhantomData<T> { return PhantomData; }

pub fn main() {
  println!("{}:\t\t\t {} | {} | {} | {} | {}", module_path!(),
    fromEither::<i32, i32>(41, Either::Left(42)),
    fromOnlyLeft::<i32, i32>(OnlyLeft::Left(42)),
    fromOnlyLeftR::<i32, i32>(OnlyLeftR {left: 42, _phantom: __()}),
    fromOnlyLeft2::<i32, i32, i32>(OnlyLeft::Left(OnlyLeft::Left(42))),
    fromOnlyLeftR2::<i32, i32, i32>(OnlyLeftR
      {left: OnlyLeftR {left: 42, _phantom: __()},
      _phantom: __()}
    ),
  );
}
#-}
