id : ∀ {A : Set} → A → A
id x = x

id0 : ∀ {@0 A : Set} → A → A
id0 x = x

id⟨_⟩_ : (A : Set) → A → A
id⟨_⟩_ _ x = x

id0⟨_⟩_ : (@0 A : Set) → A → A
id0⟨_⟩_ _ x = x

idH : (A : Set) → {A} → A
idH _ {x} = x

id0H : (@0 A : Set) → {A} → A
id0H _ {x} = x

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t {} | {} | {} | {} | {} | {}", module_path!(),
    id(42),
    id0(42),
    idՖ10216Ֆ_Ֆ10217Ֆ_(42),
    id0Ֆ10216Ֆ_Ֆ10217Ֆ_(42),
    idH(42),
    id0H(42),
  );
}
#-}
