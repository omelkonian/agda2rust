id : ∀ {A : Set} → A → A
id x = x

id⟨_⟩_ : (A : Set) → A → A
id⟨_⟩_ _ x = x

idH : (A : Set) → {A} → A
idH _ {x} = x
