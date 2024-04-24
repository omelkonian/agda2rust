{-# OPTIONS --erasure #-}
module RunTimeIrrelevance where

open import Agda.Builtin.Nat using (Nat)

@0 erasedFun : Nat → Nat
erasedFun x = x

data ErasedConstructor : Set where
  @0 ir : ErasedConstructor

record ErasedField : Set where
  field x : Nat
        @0 y : Nat
open ErasedField public
