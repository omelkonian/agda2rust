open import Agda.Builtin.Nat using (Nat; _+_)

data Either (A B : Set) : Set where
  Left  : A → Either A B
  Right : B → Either A B

fromEither : ∀ {A : Set} → A → ∀ {B : Set} → Either A B → A
fromEither _    (Left  a) = a
fromEither defA (Right _) = defA

data EitherL (A B : Set) : Set where
  Left : A → EitherL A B

-- fromEither : ∀ {A : Set} → A → ∀ {B : Set} → B → Either A B → A × B
-- fromEither _ defB (Left  a) = a , defB
-- fromEither defA _ (Right b) = defA , b
