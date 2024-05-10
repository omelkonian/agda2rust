open import Agda.Builtin.Nat using (Nat; _+_)

ℕ : Set
ℕ = Nat

testAlias : ℕ
testAlias = 42

ℕ→ℕ : Set
ℕ→ℕ = ℕ → ℕ

incr : ℕ→ℕ
incr = _+ 1

testAliasF : ℕ
testAliasF = incr 41

Id : Set → Set
Id A = A

id : ∀ {A : Set} → Id A → Id A
id x = x

-- TODO: handle unused parameters in type aliases
-- probaly we want something like `Const A B = (A, std::marker::PhantomData<B>)`
Const : Set → Set → Set
Const A B = A
{-# COMPILE AGDA2RUST Const ignore #-}

idK : ∀ {A : Set} → Const A A → Const A A
idK x = x
{-# COMPILE AGDA2RUST idK ignore #-}

{-# FOREIGN AGDA2RUST
pub fn main () {
  println!("{}:\t\t {} | {} | {}", module_path!(),
    testAlias(),
    testAliasF(),
    id(42),
    // idK(42),
  );
}
#-}
