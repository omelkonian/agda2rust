module _ where

open import Agda.Builtin.Nat using (Nat; _+_; _*_)
open import Agda.Builtin.List using (List; []; _∷_)

-- variable a b : Set

-- ** Datatypes & functions

-- data Exp (v : Set) : Set where
--   Plus : Exp v → Exp v → Exp v
--   Int : Nat → Exp v
--   Var : v → Exp v
-- {-# COMPILE AGDA2RUST Exp #-}

-- eval : (a → Nat) → Exp a → Nat
-- eval env (Plus a b) = eval env a + eval env b
-- eval env (Int n) = n
-- eval env (Var x) = env x
-- {-# COMPILE AGDA2RUST eval #-}

-- ** Natural numbers

add : Nat → Nat → Nat
add x y = x + y
{-# COMPILE AGDA2RUST add #-}

-- sum : List Nat → Nat
-- sum []       = 0
-- sum (x ∷ xs) = x + sum xs
-- {-# COMPILE AGDA2RUST sum #-}

-- ** Polymorphic functions

-- _++_ : List a → List a → List a
-- []       ++ ys = ys
-- (x ∷ xs) ++ ys = x ∷ (xs ++ ys)
-- {-# COMPILE AGDA2RUST _++_ #-}

-- map : (a → b) → List a → List b
-- map f [] = []
-- map f (x ∷ xs) = f x ∷ map f xs
-- {-# COMPILE AGDA2RUST map #-}

-- -- ** Lambdas

-- plus3 : List Nat → List Nat
-- plus3 = map (λ n → n + 3)
-- {-# COMPILE AGDA2RUST plus3 #-}

-- doubleLambda : Nat → Nat → Nat
-- doubleLambda = λ a b → a + 2 * b
-- {-# COMPILE AGDA2RUST doubleLambda #-}
