open import Agda.Primitive using (Level)
open import Agda.Builtin.Nat using (Nat)

{-# FOREIGN AGDA2RUST
#[derive(Debug)]
#-}
data Maybe {a} (A : Set a) : Set a where
  Nothing : Maybe A
  Just : A → Maybe A

private variable a : Level; A : Set a

idMaybe : Maybe A → Maybe A
idMaybe x = x

m0 m1 : Maybe Nat
m0 = Nothing
m1 = Just 42

fromMaybeNat : Maybe Nat → Nat
fromMaybeNat Nothing  = 42
fromMaybeNat (Just n) = n

fromMaybe : ∀ {a}{A : Set a} → A → Maybe A → A
fromMaybe def Nothing  = def
fromMaybe _   (Just x) = x

{-# FOREIGN AGDA2RUST
use self::Maybe::{Nothing,Just};

pub fn main () {
  println!("{}:\t\t\t {:?} | {:?} | {} | {} | {} | {} | {}", module_path!(),
    idMaybe(Just(42)), idMaybe(m1()),
    fromMaybeNat(Nothing()), fromMaybeNat(m0()), fromMaybeNat(m1()),
    fromMaybe(42, m0()), fromMaybe(0, m1()),
  );
}
#-}
