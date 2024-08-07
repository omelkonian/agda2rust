open import Agda.Primitive using (Level)

private variable
  ℓ : Level
  A B : Set ℓ
  a : A
  b : B

data Bottom (A : Set ℓ) : Set ℓ where

testBottom : Bottom A → B
testBottom ()

record Empty (A : Set ℓ) : Set ℓ where

testEmpty : Empty A → Empty B
testEmpty record {} = record {}

data TheBottom (A : Set ℓ) (a : A) : Set ℓ where

-- testTheBottom : TheBottom A a → TheBottom B b
-- testTheBottom ()

-- record TheEmpty (A : Set ℓ) (a : A) : Set ℓ where

-- testTheEmpty : TheEmpty A a → TheEmpty B b
-- testTheEmpty record {} = record {}

-- data Top (A : Set ℓ) : Set ℓ where
--   tt : @0 A → Top A

-- testTop : Top A → Top A
-- testTop (tt x) = tt x

-- record Unit (A : Set ℓ) : Set ℓ where
--   field the : A

-- testUnit : Unit A → Unit A
-- testUnit record {the = a} = record {the = a}

-- data TheTop (A : Set ℓ) : A → Set ℓ where
--   tt : (@0 a : A) → TheTop A a

-- testTheTop : TheTop A a → TheTop A a
-- testTheTop (tt a) = tt a

-- record TheUnit (A : Set ℓ) (a : A) : Set ℓ where
--   field the : A

-- testTheUnit : TheUnit A a → TheUnit A a
-- testTheUnit record {the = a} = record {the = a}

-- data TheTop2 (A : Set ℓ) : A → Set ℓ where
--   tt : (a : A) → TheTop2 A a

-- testTheTop2 : TheTop2 A a → TheTop2 A a
-- testTheTop2 (tt a) = tt a

-- data TheTop3 (A : Set ℓ) : A → Set ℓ where
--   tt : (@0 a : A) → A → TheTop3 A a

-- testTheTop3 : TheTop3 A a → TheTop3 A a
-- testTheTop3 (tt a0 a) = tt a0 a

{-# FOREIGN AGDA2RUST
use std::marker::{PhantomData};
fn __<T>() -> PhantomData<T> { return PhantomData; }

pub fn main () {
  println!("{}:\t {}", module_path!(),
    testBottom(__()),
  );
}
#-}
