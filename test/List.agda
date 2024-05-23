open import Agda.Builtin.List using (List; []; _∷_)
{-# FOREIGN AGDA2RUST
#[path = "Agda/Builtin/List.rs"] mod ListMod;
use self::ListMod::List;
#-}

open import Agda.Builtin.Nat using (Nat; _+_)

sum : List Nat → Nat
sum [] = 0
sum (x ∷ xs) = x + sum xs

pattern [_⨾_] x y = x ∷ y ∷ []

testSum : Nat
testSum = sum [ 30 ⨾ 12 ]

private variable A B C : Set

_++_ : List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

map : (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

zipWith : (A → B → C) → List A → List B → List C
zipWith f []       _        = []
zipWith f _        []       = []
zipWith f (a ∷ as) (b ∷ bs) = f a b ∷ zipWith f as bs

{-# FOREIGN AGDA2RUST
fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

use self::List::{Ֆ91ՖՖ93Ֆ,_Ֆ8759Ֆ_};

pub fn main() {
  println!("{}:\t\t\t {} | {} | {}", module_path!(),
    testSum(),
    sum(map(|x| x + 1, _Ֆ43ՖՖ43Ֆ_(
     _Ֆ8759Ֆ_(39, ᐁ(Ֆ91ՖՖ93Ֆ())),
     _Ֆ8759Ֆ_(1, ᐁ(Ֆ91ՖՖ93Ֆ()))
    ))),
    sum(zipWith(|x, y| x + y,
     _Ֆ8759Ֆ_(40, ᐁ(Ֆ91ՖՖ93Ֆ())),
     _Ֆ8759Ֆ_(2, ᐁ(Ֆ91ՖՖ93Ֆ()))
    )),
  );
}
#-}
