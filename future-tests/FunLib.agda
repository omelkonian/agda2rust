open import Agda.Primitive using (Level)
open import Agda.Builtin.Nat using (Nat; _+_)

private
  variable
    -- a b c d e : Level
    -- A : Set a
    -- B : Set b
    -- C : Set c
    -- D : Set d
    -- E : Set e
    A B C D E : Set

id : A → A
id x = x

-- const : A → B → A
-- const x = λ _ → x

constᵣ1 : A → B → B
constᵣ1 _ = λ x → x

constᵣ2 : A → B → B
constᵣ2 _ = id

-- {-# FOREIGN AGDA2RUST
-- pub fn main () {
--   println!("{}:\t {} | {} | {}", module_path!(),
--     id(42),
--     r#const(42, 0),
--     constᵣ(0, 42),
--   );
-- }
-- #-}

-- infixr 9 _∘_ _∘₂_
-- infixl 8 _ˢ_
-- infixl 0 _|>_
-- infix  0 case_return_of_
-- infixr -1 _$_

-- _∘_ : ∀ {A : Set a} {B : A → Set b} {C : {x : A} → B x → Set c} →
--       (∀ {x} (y : B x) → C y) → (g : (x : A) → B x) →
--       ((x : A) → C (g x))
-- f ∘ g = λ x → f (g x)
-- {-# INLINE _∘_ #-}

-- _∘₂_ : ∀ {A₁ : Set a} {A₂ : A₁ → Set d}
--          {B : (x : A₁) → A₂ x → Set b}
--          {C : {x : A₁} → {y : A₂ x} → B x y → Set c} →
--        ({x : A₁} → {y : A₂ x} → (z : B x y) → C z) →
--        (g : (x : A₁) → (y : A₂ x) → B x y) →
--        ((x : A₁) → (y : A₂ x) → C (g x y))
-- f ∘₂ g = λ x y → f (g x y)

-- flip : ∀ {A : Set a} {B : Set b} {C : A → B → Set c} →
--        ((x : A) (y : B) → C x y) → ((y : B) (x : A) → C x y)
-- flip f = λ y x → f x y
-- {-# INLINE flip #-}

-- _$_ : ∀ {A : Set a} {B : A → Set b} →
--       ((x : A) → B x) → ((x : A) → B x)
-- f $ x = f x
-- {-# INLINE _$_ #-}

-- _|>_ : ∀ {A : Set a} {B : A → Set b} →
--        (a : A) → (∀ a → B a) → B a
-- _|>_ = flip _$_
-- {-# INLINE _|>_ #-}

-- _ˢ_ : ∀ {A : Set a} {B : A → Set b} {C : (x : A) → B x → Set c} →
--       ((x : A) (y : B x) → C x y) →
--       (g : (x : A) → B x) →
--       ((x : A) → C x (g x))
-- f ˢ g = λ x → f x (g x)
-- {-# INLINE _ˢ_ #-}

-- _$- : ∀ {A : Set a} {B : A → Set b} → ((x : A) → B x) → ({x : A} → B x)
-- f $- = f _
-- {-# INLINE _$- #-}

-- λ- : ∀ {A : Set a} {B : A → Set b} → ({x : A} → B x) → ((x : A) → B x)
-- λ- f = λ x → f
-- {-# INLINE λ- #-}

-- case_returning_of_ : ∀ {A : Set a} (x : A) (B : A → Set b) →
--                   ((x : A) → B x) → B x
-- case x returning B of f = f x
-- {-# INLINE case_returning_of_ #-}

-- infixr 9 _∘′_ _∘₂′_
-- infixl 0 _|>′_
-- infix  0 case_of_
-- infixr -1 _$′_

-- _∘′_ : (B → C) → (A → B) → (A → C)
-- f ∘′ g = _∘_ f g

-- _∘₂′_ : (C → D) → (A → B → C) → (A → B → D)
-- f ∘₂′ g = _∘₂_ f g

-- flip′ : (A → B → C) → (B → A → C)
-- flip′ = flip

-- _$′_ : (A → B) → (A → B)
-- _$′_ = _$_

-- _|>′_ : A → (A → B) → B
-- _|>′_ = _|>_

-- case_of_ : A → (A → B) → B
-- case x of f = case x returning _ of f
-- {-# INLINE case_of_ #-}

-- infixl 1 _⟨_⟩_
-- infixl 0 _∋_

-- _⟨_⟩_ : A → (A → B → C) → B → C
-- x ⟨ f ⟩ y = f x y

-- _∋_ : (A : Set a) → A → A
-- A ∋ x = x

-- typeOf : {A : Set a} → A → Set a
-- typeOf {A = A} _ = A

-- it : {A : Set a} → {{A}} → A
-- it {{x}} = x

-- infixr 0 _-⟪_⟫-_ _-⟨_⟫-_
-- infixl 0 _-⟪_⟩-_
-- infixr 1 _-⟨_⟩-_ ∣_⟫-_ ∣_⟩-_
-- infixl 1 _on_ _on₂_ _-⟪_∣ _-⟨_∣

-- _-⟪_⟫-_ : (A → B → C) → (C → D → E) → (A → B → D) → (A → B → E)
-- f -⟪ _*_ ⟫- g = λ x y → f x y * g x y

-- _-⟪_∣ : (A → B → C) → (C → B → D) → (A → B → D)
-- f -⟪ _*_ ∣ = f -⟪ _*_ ⟫- constᵣ

-- ∣_⟫-_ : (A → C → D) → (A → B → C) → (A → B → D)
-- ∣ _*_ ⟫- g = const -⟪ _*_ ⟫- g

-- _-⟨_∣ : (A → C) → (C → B → D) → (A → B → D)
-- f -⟨ _*_ ∣ = f ∘₂ const -⟪ _*_ ∣

-- ∣_⟩-_ : (A → C → D) → (B → C) → (A → B → D)
-- ∣ _*_ ⟩- g = ∣ _*_ ⟫- g ∘₂ constᵣ

-- _-⟪_⟩-_ : (A → B → C) → (C → D → E) → (B → D) → (A → B → E)
-- f -⟪ _*_ ⟩- g = f -⟪ _*_ ⟫- ∣ constᵣ ⟩- g

-- _-⟨_⟫-_ : (A → C) → (C → D → E) → (A → B → D) → (A → B → E)
-- f -⟨ _*_ ⟫- g = f -⟨ const ∣ -⟪ _*_ ⟫- g

-- _-⟨_⟩-_ : (A → C) → (C → D → E) → (B → D) → (A → B → E)
-- f -⟨ _*_ ⟩- g = f -⟨ const ∣ -⟪ _*_ ⟫- ∣ constᵣ ⟩- g

-- _on₂_ : (C → C → D) → (A → B → C) → (A → B → D)
-- _*_ on₂ f = f -⟪ _*_ ⟫- f

-- _on_ : (B → B → C) → (A → B) → (A → A → C)
-- _*_ on f = f -⟨ _*_ ⟩- f
