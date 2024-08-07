open import Agda.Builtin.Nat using (Nat; _+_)

swapF : ∀ {A B : Set} → (A → A → B) → (A → A → B)
swapF f x y = f y x

id : ∀ {A : Set} → A → A
id x = x

add3 : Nat → Nat → (Nat → Nat)
add3 x y = λ z → x + y + z

-- TODO: issue with closures
testSwapId3 : Nat
testSwapId3 = id swapF add3 40 1 1
{- argTypes(id)
   ------------
   ≪id :  ∀ {A} → A → A
     ≈ [A]
     swapF : ∀ {A B} → (A → A → B) → A → A → B
     add3  : Nat → Nat → Nat → Nat
     40    : Nat
     1     : Nat
     1     : Nat
   id · swapF : ∀ {A B} → (A → A → B) → A → A → B
     ↔ A₀ ≟ (∀ {A B} → (A → A → B) → A → A → B) ⟪ A = ∀ ... ⟫
     ≈ ◇[A → A → B, A, A]
   id · swapF · add3 : Nat → Nat → Nat → Nat
     ↔ (A → A → B) ≟ (Nat → Nat → Nat → Nat) ⟪ A = Nat, B = Nat → Nat ⟫
     ≈ [A, A]◇[A]
   id · swapF · add3 · 40 : Nat → Nat → Nat
     ↔ A ≟ Nat
     ≈ [A]◇[A]
   id · swapF · add3 · 40 · 1 : Nat → Nat
     ↔ A ≟ Nat
     ≈ [A]
   id · swapF · add3 · 40 · 1 · 1 : Nat
     ↔ A ≟ Nat
     ≈ []

   return[
     0(swapF): A₀ := ∀ {A B} → (A → A → B) → A → A → B
     1(add3): (A → A → B) := Nat → Nat → (Nat → Nat)
     2(40) : A
     3(1) : A
     4(1) : A
   ]
-}

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t\t {}", module_path!(),
    testSwapId3(),
  );
}
#-}
