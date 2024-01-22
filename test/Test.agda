module _ where

open import Agda.Builtin.Nat using (Nat; _+_; _*_)
open import Agda.Builtin.List using (List; []; _∷_)

variable a b : Set

-- ** Natural numbers

answer : Nat
answer = 42

suc : Nat → Nat
suc x = x + 1

add_answer : Nat → Nat
add_answer x = x + answer

add : Nat → Nat → Nat
add x y = x + y

add3 : Nat → Nat → Nat → Nat
add3 x y z = x + y + z

add3b : Nat → Nat → Nat → Nat
add3b x y z = add x (add y z)

-- sum : List Nat → Nat
-- sum []       = 0
-- sum (x ∷ xs) = x + sum xs

-- ** Datatypes & functions

data Maybe (A : Set) : Set where
  Nothing : Maybe A
  Just : A → Maybe A

m0 : Maybe Nat
m0 = Nothing

m1 : Maybe Nat
m1 = Just 1

fromMaybeNat : Maybe Nat → Nat
fromMaybeNat Nothing  = 0
fromMaybeNat (Just n) = n

maybeToBool : Maybe Nat → Nat
maybeToBool Nothing  = 0
maybeToBool _ = 1

-- fromMaybe : ∀ {A : Set} → A → Maybe A → A
-- fromMaybe def Nothing  = def
-- fromMaybe _   (Just x) = x

-- data Maybe' {ℓ} (A : Set ℓ) : Set ℓ where
--   Nothing : Maybe' A
--   Just : A → Maybe' A

-- m0' : Maybe' Nat
-- m0' = Nothing

-- m1' : Maybe' Nat
-- m1' = Just 1

-- fromMaybe' : ∀ {ℓ} {A : Set ℓ} → A → Maybe' A → A
-- fromMaybe' def Nothing  = def
-- fromMaybe' _   (Just x) = x

-- data Exp (V : Set) : Set where
--   Plus : Exp V → Exp V → Exp V

-- eval : Exp a → Nat
-- eval (Plus a b) = eval a + eval b

-- data Exp (V : Set) : Set where
--   Plus : Exp V → Exp V → Exp V
--   Int : Nat → Exp V
--   Var : V → Exp V

-- eval : (a → Nat) → Exp a → Nat
-- eval env (Plus a b) = eval env a + eval env b
-- eval env (Int n) = n
-- eval env (Var x) = env x

-- ** Polymorphic functions

-- _++_ : List a → List a → List a
-- []       ++ ys = ys
-- (x ∷ xs) ++ ys = x ∷ (xs ++ ys)

-- map : (a → b) → List a → List b
-- map f [] = []
-- map f (x ∷ xs) = f x ∷ map f xs

-- -- ** Lambdas

-- plus3 : List Nat → List Nat
-- plus3 = map (λ n → n + 3)

-- doubleLambda : Nat → Nat → Nat
-- doubleLambda = λ a b → a + 2 * b
