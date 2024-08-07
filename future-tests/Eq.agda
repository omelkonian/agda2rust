open import Agda.Primitive using (Level; lzero)
open import Agda.Builtin.Nat using (Nat; suc)

private variable ℓ : Level

{-# FOREIGN AGDA2RUST
#[derive(Debug)]
#-}
infix 4 _≡_
data _≡_ {A : Set ℓ} (x : A) : A → Set ℓ where
  instance refl : x ≡ x

eqZero : 0 ≡ 0
eqZero = refl

private variable A B C : Set ℓ

cong : ∀ {a b : A} (f : A → B) → a ≡ b → f a ≡ f b
cong f refl = refl

EqNat = _≡_ {A = Nat}

eqOne : EqNat 1 1
eqOne = cong suc eqZero

sym : {a b : A} → a ≡ b → b ≡ a
sym refl = refl

infixr 9 _∘_
infixr -1 _$_

_∘_ : (B → C) → (A → B) → (A → C)
(f ∘ g) x = f (g x)

_$_ : (A → B) → A → B
f $ x = f x

eqTwo : EqNat 2 2
eqTwo = sym $ cong (suc ∘ suc) eqZero

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


-- eqLevel : lzero ≡ lzero
-- eqLevel = refl

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t {:?} | {:?}", module_path!(),
    eqZero(),
    eqOne(),
  );
}
#-}
