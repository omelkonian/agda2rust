open import Agda.Builtin.Nat using (Nat; _+_)

private variable A : Set

data Exp (V : Set) : Set where
  Plus : Exp V → Exp V → Exp V

eval : Exp A → Nat
eval (Plus a b) = eval a + eval b

-- data Exp (V : Set) : Set where
--   Plus : Exp V → Exp V → Exp V
--   Int : Nat → Exp V
--   Var : V → Exp V

-- eval : (a → Nat) → Exp a → Nat
-- eval env (Plus a b) = eval env a + eval env b
-- eval env (Int n) = n
-- eval env (Var x) = env x
