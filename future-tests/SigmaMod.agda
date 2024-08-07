module _ where

open import Agda.Builtin.Sigma using (Σ; _,_)
open import Agda.Builtin.Nat using (Nat)

module _ {a b}{A : Set a}{B : A → Set b} where
  fst : Σ A B → A
  fst record {fst = a} = a

  snd : (p : Σ A B) → B (fst p)
  snd record {snd = b} = b

pairOfNats : Σ Nat (λ _ → Nat)
pairOfNats = record {fst = 0; snd = 42}

module _ {a b}{A : Set a}{B : Set b} where
  swap : Σ A (λ _ → B) → Σ B (λ _ → A)
  swap (a , b) = (b , a)

-- const : ∀ {A : Set} → Set → (A → Set)
-- const B = λ _ → B

-- swap' : Σ A (const B) → Σ B (const A)
-- swap' (a , b) = (b , a)
