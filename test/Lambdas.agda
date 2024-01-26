open import Agda.Builtin.Nat using (Nat; _+_; _*_)
open import Agda.Builtin.List using (List)
open import List using (map)

plus3 : List Nat → List Nat
plus3 = map (λ n → n + 3)

doubleLambda : Nat → Nat → Nat
doubleLambda = λ a b → a + 2 * b
