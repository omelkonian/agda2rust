module NonLinearFun where

open import Agda.Builtin.Nat using (Nat; _+_)

double : Nat → Nat
double x = x + x

-- data One : Set where
--   ◇ : One

-- data Two : Set where
--   ◆ : One → One → Two

-- double◇ : One → Two
-- double◇ ◇ = ◆ ◇ ◇

{-# FOREIGN AGDA2RUST #[derive(Debug)] #-}
data List {a} (A : Set a) : Set a where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

infixr 4 _∷_
private variable A B : Set

stutter : List A → List A
stutter [] = []
stutter (x ∷ xs) = x ∷ x ∷ stutter xs
