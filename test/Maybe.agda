open import Agda.Builtin.Nat using (Nat)

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

fromMaybe : ∀ {A : Set} → A → Maybe A → A
fromMaybe def Nothing  = def
fromMaybe _   (Just x) = x

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
