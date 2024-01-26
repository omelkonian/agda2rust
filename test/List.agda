open import Agda.Builtin.List using (List; []; _∷_)

private variable a b : Set

_++_ : List a → List a → List a
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

map : (a → b) → List a → List b
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs
