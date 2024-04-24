module Simplification where

open import Agda.Builtin.Nat using (Nat; _+_)

increment : Nat → Nat
increment = _+ 1

-- NB: when the `simplifyTTerm` optimization pass is enabled, we get `1 + x0`
