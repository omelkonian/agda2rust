module FieldVisibility where

open import Agda.Builtin.Nat using (Nat)

record R1 : Set where
  field x : Nat
open R1 public

record R2 : Set where
  field x : Nat
open R2 public

x1 : R1 → Nat
x1 = x

x2 : R1 → Nat
x2 = x
