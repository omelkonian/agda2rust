{-# FOREIGN AGDA2RUST #[derive(Debug)] #-}
data List {a} (A : Set a) : Set a where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

private variable A B : Set

_++_ : List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

map : (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs
