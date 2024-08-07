Id : Set → Set
Id A = A

id : ∀ {A : Set} → Id A → Id A
id x = x

-- k : ∀ {A : Set} → Set → (A → Set)
-- k B = λ _ → B
-- {-# COMPILE AGDA2RUST k ignore #-}

-- open import Agda.Primitive using (Level)
-- private variable ℓ : Level; A : Set ℓ

-- id : ∀ {A : Set} → k A A → k A A
-- id x = x

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}: {}", module_path!(),
    id(42),
  );
}
#-}
