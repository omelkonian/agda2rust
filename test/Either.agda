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
