open import Agda.Builtin.Nat using (Nat; _+_)

double : Nat → Nat
double x = x + x

-- data One : Set where
--   ◇ : One

-- data Two : Set where
--   ◆ : One → One → Two

-- double◇ : One → Two
-- double◇ ◇ = ◆ ◇ ◇

{-# FOREIGN AGDA2RUST
#[derive(Debug)]
#-}
data List {a} (A : Set a) : Set a where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

infixr 4 _∷_
private variable A B : Set

stutter : List A → List A
stutter [] = []
stutter (x ∷ xs) = x ∷ x ∷ stutter xs

{-# FOREIGN AGDA2RUST
fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

pub fn main() {
  println!("{}:\t\t {:?}", module_path!(),
    stutter(List::_Ֆ8759Ֆ_(4, ᐁ(List::Ֆ91ՖՖ93Ֆ()))),
  );
}
#-}

{-
double : Nat → Nat
double x = x + x

doubleF : {A : Set} {-: Copy -} → (A → A → A) → A → A
doubleF f x = f x x

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t {}", module_path!(),
    double(21),
    apply!(doubleF, ᐁF(move |x| ᐁF(move |y| x + y)), 21),
  );
}
#-}

-- twice : {A : Set} → (A → A) → A → A
-- twice f x = f (f x)

-}
