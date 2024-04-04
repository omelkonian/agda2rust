infix 4 _≡_
data _≡_ {a} {A : Set a} (x : A) : A → Set a where
  instance refl : x ≡ x

private variable A B : Set

cong : ∀ {a b : A} (f : A → B) → a ≡ b → f a ≡ f b
cong f refl = refl

sym : {a b : A} → a ≡ b → b ≡ a
sym refl = refl

trans : {a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans refl refl = refl

-- subst : ∀ {P : A → Set} {a b : A} → a ≡ b → P a → P b
-- subst refl p = p

-- add-left-id : (a : Nat) → Eq (add Zero a) a
-- add-left-id Zero = Refl
-- add-left-id (Succ a) = cong Succ (add-left-id a)

-- succ-left-add : (a b : Nat) → Eq (add (Succ a) b) (Succ (add a b))
-- succ-left-add a Zero = Refl
-- succ-left-add a (Succ b) = cong Succ (succ-left-add a b)

-- add-comm : ∀ (a b : Nat) → Eq (add a b) (add b a)
-- add-comm a Zero = sym (add-left-id a)
-- add-comm a (Succ b) = trans (cong Succ (add-comm a b)) (sym (succ-left-add b a))
