id : ∀ {A : Set} → A → A
id x = x

id⟨_⟩_ : (A : Set) → A → A
id⟨_⟩_ _ x = x

idH : (A : Set) → {A} → A
idH _ {x} = x

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t\t {} | {} | {}", module_path!(),
    id(42),
    idՖ10216Ֆ_Ֆ10217Ֆ_(42),
    idH(42)
  );
}
#-}
