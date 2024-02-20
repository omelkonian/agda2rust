open import Agda.Builtin.Nat using (Nat; _+_)

data Exp (V : Set) : Set where
  Plus : Exp V → Exp V → Exp V
  Int : Nat → Exp V
  Var : V → Exp V

private variable A : Set

eval : (A → Nat) → Exp A → Nat
eval env (Plus a b) = eval env a + eval env b
eval env (Int n) = n
eval env (Var x) = env x
