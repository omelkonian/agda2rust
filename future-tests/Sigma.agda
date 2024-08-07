open import Agda.Primitive using (Level; _⊔_)

private variable a b : Level

record Σ (A : Set a) (B : A → Set b) : Set (a ⊔ b) where
  constructor _,_
  field
    fst : A
    snd : B fst
open Σ public

infixr 4 _,_

private variable A : Set a; B : Set b

swap : Σ A (λ _ → B) → Σ B (λ _ → A)
swap (a , b) = (b , a)

open import Agda.Builtin.Nat using (Nat; _+_)

pairOfNats : Σ Nat (λ _ → Nat)
pairOfNats = record {fst = 0; snd = 42}

NatPair = Σ Nat (λ _ → Nat)

sum : NatPair → Nat
sum (x , y) = x + y

exPair : NatPair
exPair = 22 , 20
{-# COMPILE AGDA2RUST exPair const #-}

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}: {} | {} | {} | {}", module_path!(),
    sum(Σ {fst: 22, snd: 20}),
    sum(swap(Σ {fst: 22, snd: 20})),
    sum(exPair),
    sum(swap(exPair)),
  );
}
#-}
