open import Agda.Primitive using (Level)
open import Agda.Builtin.Nat using (suc; _+_) renaming (Nat to ℕ)
-- open import Agda.Builtin.Int using () renaming (Int to ℤ)
-- open import Agda.Builtin.String using (String)

private variable
  ℓ : Level
  A : Set ℓ

data Wrap {ℓ} (A : Set ℓ) : Set ℓ where
  mk : A → Wrap A

unmk : Wrap A → A
unmk (mk a) = a

record Point : Set where
  field slot      : Wrap ℕ
        blockHash : ℕ
open Point public

matchPoint : Point → ℕ
matchPoint record {slot = mk n} = n

exPoint : Point
exPoint .slot      = mk 42
exPoint .blockHash = 0

Hash = ℕ

{-# FOREIGN AGDA2RUST
#[derive(Default)]
#-}
record Header : Set where
  field  slotNo blockNo : ℕ
         blockHash      : Hash
         prev nodeId    : ℕ
open Header public

matchHeader : Header → ℕ
matchHeader record {slotNo = n} = n

exHeader : Header
exHeader = λ where
  .slotNo → 42
  .blockNo → 0
  .blockHash → 0
  .prev → 0
  .nodeId → 0

data Tx : Set where
  inc dec : Tx

pred : ℕ → ℕ
pred 0 = 0
pred (suc n) = n

-- applyTx : Tx → ℕ → ℕ
-- applyTx = λ where
--   inc → suc
--   dec → pred

-- applyTx : Tx → ℕ → ℕ
-- applyTx inc = suc
-- applyTx dec = pred

-- ℕ→ℕ = ℕ → ℕ

-- applyTx : Tx → ℕ→ℕ
-- applyTx = λ where
--   inc → _+ 1
--   dec → _- 1
--  where
--   _-_ : ℕ → ℕ → ℕ
--   0 - _ = 0
--   n - 0 = n
--   (suc x) - (suc y) = x - y

-- postulate @0 List : Set → Set

-- record Block : Set where
--   field header  : Header
--         @0 body : List Tx

-- record LedgerState : Set where
--   field  tip                  : Point
--          count                : ℤ
--          snapshot1 snapshot2  : ℤ

-- postulate hash : String → Hash

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t {} | {} | {} | {}", module_path!(),
    unmk(Point·slot(exPoint())),
    matchPoint(exPoint()),
    Header·slotNo(Header {slotNo: 42, .. Default::default()}),
    matchHeader(exHeader()),
  );
}
#-}
