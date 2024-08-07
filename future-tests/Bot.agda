open import Agda.Primitive using (Level)

private variable ℓ ℓ′ : Level

data Bot : Set where

bot-elim : ∀ {A : Set ℓ} → Bot → A
bot-elim ()

postulate deus-ex-machina : Bot

open import Agda.Builtin.Nat using () renaming (Nat to ℕ)

testBot : ℕ
testBot = bot-elim deus-ex-machina

-- testBot : ℕ
-- testBot = bot-elim deus-ex-machina

-- data BotPoly {ℓ} : Set ℓ where

-- botP-elim : ∀ {A : Set ℓ} → BotPoly {ℓ′} → A
-- botP-elim ()

-- data BotIndexed (A : Set) : Set where

-- botI-elim : ∀ {A : Set} → BotIndexed A → A
-- botI-elim ()

-- -- ** TErased??
-- -- botI-elim' : ∀ {A : Set} → BotIndexed A → BotIndexed A
-- -- botI-elim' ()

{-# FOREIGN AGDA2RUST
use std::marker::{PhantomData};
fn __<T>() -> PhantomData<T> { return PhantomData; }

pub fn main () {
  println!("{}:\t {}", module_path!(),
    testBottom(__()),
  );
}
#-}
