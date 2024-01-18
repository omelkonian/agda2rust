module _ where

open import Agda.Builtin.Nat using (Nat; _+_; _*_)
open import Agda.Builtin.List using (List; []; _∷_)

variable a b : Set

-- ** Natural numbers

answer : Nat
answer = 42

suc : Nat → Nat
suc x = x + 1

addAnswer : Nat → Nat
addAnswer x = x + answer

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

data Exp (v : Set) : Set where
  Plus : Exp v → Exp v → Exp v
  Int : Nat → Exp v
  Var : v → Exp v

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
