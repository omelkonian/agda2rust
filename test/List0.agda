{-# FOREIGN AGDA2RUST #[derive(Debug)] #-}
data List {a} (A : Set a) : Set a where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

private variable A B : Set

_++_ : List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

map : (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

{-# FOREIGN AGDA2RUST
fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

use self::List::{Ֆ91ՖՖ93Ֆ,_Ֆ8759Ֆ_};

pub fn main() {
  println!("{}:\t\t\t {:?}", module_path!(),
    map(|x| x + 1, _Ֆ43ՖՖ43Ֆ_(
      _Ֆ8759Ֆ_(3, ᐁ(Ֆ91ՖՖ93Ֆ())),
      _Ֆ8759Ֆ_(1, ᐁ(Ֆ91ՖՖ93Ֆ()))
    )),
  );
}
#-}
