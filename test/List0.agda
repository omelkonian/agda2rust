{-# FOREIGN AGDA2RUST #[derive(Debug)] #-}
data List {a} (A : Set a) : Set a where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A
infixr 4 _∷_

private variable A B : Set

_++_ : List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

map : (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

flatten : List (List A) → List A
flatten [] = []
flatten (xs ∷ xss) = xs ++ flatten xss

head : List (List A) → List A
head [] = []
head ([] ∷ _) = []
head ((x ∷ _) ∷ _) = x ∷ []

open import Agda.Builtin.Nat using (Nat; _+_)

empty : List Nat
empty = []
{-# COMPILE AGDA2RUST empty const #-}

headNum : List Nat → Nat
headNum [] = 42
headNum (x ∷ _) = x

single : Nat → List Nat
single x = x ∷ []

sum : List Nat → Nat
sum [] = 0
sum (x ∷ xs) = x + sum xs

pattern [_] x = x ∷ []
pattern [_⨾_] x y = x ∷ [ y ]
pattern [_⨾_⨾_] x y z = x ∷ [ y ⨾ z ]

testFlatten : Nat
testFlatten = sum (flatten [ [ 20 ] ⨾ [ 20 ] ⨾ [ 2 ] ])

testHead : List Nat
testHead = head [ [ 42 ⨾ 2 ⨾ 3 ] ⨾ [] ⨾ [] ]

{-# FOREIGN AGDA2RUST
fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

use self::List::{Ֆ91ՖՖ93Ֆ,_Ֆ8759Ֆ_};

pub fn main() {
  println!("{}:\t\t\t {:?} | {:?} | {:?} | {} | {:?}", module_path!(),
    headNum(empty),
    single(42),
    map(|x| x + 1, _Ֆ43ՖՖ43Ֆ_(
      _Ֆ8759Ֆ_(3, ᐁ(Ֆ91ՖՖ93Ֆ())),
      _Ֆ8759Ֆ_(1, ᐁ(Ֆ91ՖՖ93Ֆ()))
    )),
    testFlatten(),
    testHead(),
  );
}
#-}
