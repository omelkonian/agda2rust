open import Agda.Builtin.Nat using (Nat)

.irrFun : Nat → Nat
irrFun x = x

record IrrField : Set where
  field x : Nat
        .y : Nat
open IrrField public
