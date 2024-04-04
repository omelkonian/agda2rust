open import Agda.Builtin.Equality

private variable A B : Set

cong : ∀ {a b : A} (f : A → B) → a ≡ b → f a ≡ f b
cong f refl = refl

sym : {a b : A} → a ≡ b → b ≡ a
sym refl = refl

trans : {a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans refl refl = refl
