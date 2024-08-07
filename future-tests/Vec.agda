{-# FOREIGN AGDA2RUST
#[derive(Debug)]
#-}
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

private variable n m : Nat

{-# FOREIGN AGDA2RUST
#[derive(Debug)]
#-}
data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : (x : A) (xs : Vec A n) → Vec A (suc n)

private variable A B : Set

-- vectorLength' : Vec A n → Nat
-- vectorLength' [] = zero
-- vectorLength' (_ ∷ xs) = suc (vectorLength' xs)

vectorLength : ∀ {A : Set} {n : Nat}
             → Vec A n → Nat
vectorLength {n = n} _ = n

nilLength : Nat → Nat
nilLength n = vectorLength {A = Vec Nat n} []

-- zeroLength≡ : vectorLength (Vec A n ∋ []) ≡ 0
-- zeroLength≡ = refl

-- map : (A → B) → Vec A n → Vec B n
-- map f [] = []
-- map f (x ∷ xs) = f x ∷ map f xs

-- _+_ : Nat → Nat → Nat
-- zero  + m = m
-- suc n + m = suc (n + m)

-- _++_ : Vec A n → Vec A m → Vec A (n + m)
-- []       ++ ys = ys
-- (x ∷ xs) ++ ys = x ∷ (xs ++ ys)

-- head : Vec A (suc n) → A
-- head (x ∷ _) = x

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{:?}", nilLength(Nat::zero()));
}
#-}
