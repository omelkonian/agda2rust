-- {-# OPTIONS -v treeless.opt.erase.info:100 #-}
module Erasure where

-- ** Skip pattern lambdas and with-generated functions.
data II : Set where
  ◇ ◆ : II

ii ii' : II
ii  = ◇
ii' = ◆

II→II : II → II
II→II ◇ = ◆
II→II ◆ = ◇

II→II' : II → II
II→II' = λ where
  ◇ → ◆
  ◆ → ◇

II→II'' : II → II
II→II'' x with x
... | ◇ = ◆
... | ◆ = ◇

-- ** Do not erase unit-like types/values.
data I : Set where
  ◇ : I

i : I
i = ◇

-- I→II : I → II
-- I→II ◇ = ◇

-- II→I : II → I
-- II→I _ = ◇
