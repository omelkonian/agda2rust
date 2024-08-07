open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)

open import Agda.Builtin.List using (List; []; _∷_)
{-# FOREIGN AGDA2RUST
#[path = "Agda/Builtin/List.rs"] mod ListMod;
use self::ListMod::List;
#-}

-- suc : Nat → Nat
dec : Nat → Nat
dec = λ where
  zero    → zero
  (suc n) → n

ListOfClosures : Set
ListOfClosures = List (Nat → Nat)

private variable A : Set

head : A → List A → A
head def = λ where
  [] → def
  (x ∷ _) → x

test : Nat
test =
  head (λ _ → 0)
       ((λ x → x + 1) ∷ (λ where 0 → 0; (suc n) → n) ∷ [])
       41
