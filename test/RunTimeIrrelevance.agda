open import Agda.Builtin.Nat using (Nat; suc)

-- ** functions

@0 erasedFun : Nat → Nat
erasedFun x = x

module @0 ErasedModule where postulate
  _≡_ : Nat → Nat → Set
  _≤_ : Nat → Nat → Set
open ErasedModule

erasedFunArg : (n : Nat) → @0 (n ≡ 0) → Nat
erasedFunArg n _ = suc n

erasedHigherOrderFunArg : @0 Nat → (@0 Nat → Nat) → Nat
erasedHigherOrderFunArg n f = suc (f n)

-- ** records

record @0 ErasedRec (x y : Nat) : Set where
  field x≡y : x ≡ y

erasedRec : {@0 x y : Nat} → @0 ErasedRec x y → Nat
erasedRec _ = 42

record ErasedField : Set where
  field x : Nat
        @0 x≡0 : x ≡ 0

succ : ErasedField → Nat
succ (record {x = x}) = suc x

record ErasedRecParam (@0 x : Nat) : Set where
  field y : Nat
        @0 x≡y : x ≡ y

erasedRecParam : (@0 x : Nat) → ErasedRecParam x → Nat
erasedRecParam _ (record {y = y}) = y

erasedRecParamH : {@0 x : Nat} → ErasedRecParam x → Nat
erasedRecParamH (record {y = y}) = y

-- ** datatypes

data @0 ErasedData : Set where
  mk : (m n : Nat) → m ≡ n → ErasedData

erasedData : @0 ErasedData → Nat
erasedData (mk m n _) = 42

{-# FOREIGN AGDA2RUST #[derive(Debug)] #-}
data ErasedCon : Set where
  mk : Nat → ErasedCon
  @0 mkIrr : (m : Nat) → m ≡ 0 → ErasedCon

erasedClause : ErasedCon → Nat
erasedClause (mk n) = n
erasedClause (mkIrr n _) = n

{-# FOREIGN AGDA2RUST #[derive(Debug)] #-}
data ErasedConArg : Set where
  mk : (n : Nat) → @0 (n ≡ 0) → ErasedConArg

erasedConArg : ErasedConArg → Nat
erasedConArg (mk n _) = n

{-# FOREIGN AGDA2RUST #[derive(Debug)] #-}
data BST (@0 lower upper : Nat) : Set where
  Leaf : (@0 pf : lower ≤ upper) → BST lower upper
  Node : (x : Nat) (l : BST lower x) (r : BST x upper) → BST lower upper

{-# FOREIGN AGDA2RUST
use self::BST::{Leaf,Node};

pub fn main() {
  println!("{}:\t \
    {} | {} | \
    {} | {} | {} | {} | \
    {} | {:?} | {:?} | {:?}", module_path!(),
    erasedFunArg(41),
    erasedHigherOrderFunArg(41),

    erasedRec(),
    succ(ErasedField {x: 41}),
    erasedRecParam(ErasedRecParam {y: 42}),
    erasedRecParamH(ErasedRecParam {y: 42}),

    erasedData(),
    erasedClause(ErasedCon::mk(42)),
    erasedConArg(ErasedConArg::mk(42)),
    Node(4, ᐁ(Node(2, ᐁ(Leaf()), ᐁ(Leaf()))), ᐁ(Leaf())),
  );
}
#-}
