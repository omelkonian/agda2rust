open import Agda.Builtin.List using (List; []; _∷_)
{-# FOREIGN AGDA2RUST
#[path = "Agda/Builtin/List.rs"] mod ListMod;
use self::ListMod::List;
#-}

private variable A B C : Set

module _ (f : A → B) where
  map : List A → List B
  map [] = []
  map (x ∷ xs) = f x ∷ map xs

module _ (f : A → B → C) where
  zipWith : List A → List B → List C
  zipWith []       _        = []
  zipWith _        []       = []
  zipWith (a ∷ as) (b ∷ bs) = f a b ∷ zipWith as bs

module _ (f : A → B → C) where
  zipWith2 : List A → List B → List C
  zipWith2 = λ where
    []       _        → []
    _        []       → []
    (a ∷ as) (b ∷ bs) → f a b ∷ zipWith2 as bs
