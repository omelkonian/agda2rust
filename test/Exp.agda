open import Agda.Builtin.Nat using (Nat; _+_)

data Exp (V : Set) : Set where
  Plus : Exp V → Exp V → Exp V
  Int  : Nat → Exp V
  Var  : V → Exp V

exampleExp : Exp Nat
exampleExp = Plus (Int 5) (Var 37)

private variable A : Set

eval : (A → Nat) → Exp A → Nat
eval env (Plus a b) = eval env a + eval env b
eval env (Int n)    = n
eval env (Var x)    = env x

-- data Bool : Set where
--   T F : Bool

-- data GExp : Set → Set₁ where
--   -- ** constants
--   nat  : Nat  → GExp Nat
--   bool : Bool → GExp Bool
--   -- ** variables
--   var  : Nat → GExp Nat
--   -- ** compound expressions
--   plus : GExp Nat → GExp Nat → GExp Nat
--   if   : GExp Bool → GExp A → GExp A → GExp A

-- geval : (Nat → Nat) → GExp A → A
-- geval env (nat n)    = n
-- geval env (bool b)   = b
-- geval env (var x)    = env x
-- geval env (plus a b) = geval env a + geval env b
-- geval env (if b l r) with geval env b
-- ... | T = geval env l
-- ... | F = geval env r

{-# FOREIGN AGDA2RUST
fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

use self::Exp::{Plus,Int,Var};

pub fn main() {
  println!("{}: {} | {}", module_path!(),
    eval(|x| x, exampleExp()),
    eval(|x| x + 1, Plus(ᐁ(Int(5)),ᐁ(Var(36)))),
  );
}
#-}
