open import Agda.Builtin.Nat using ()

{-# FOREIGN AGDA2RUST #[derive(Debug)] #-}
data The {ℓ}{A : Set ℓ} : A → Set ℓ where
  the : (a : A) → The a

_ : The 42
_ = the 42
