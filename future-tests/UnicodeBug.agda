open import Agda.Builtin.Nat using (Nat)

-- ∙ `unicode-data` gives:
--    > isXIDStart 𝟙 ↝ False
--    > isXIDContinue 𝟙 ↝ True

-- ∙ but `language-rust` says:
--    > invalid AST (identifier `_𝟙' does not lex properly)

-- ∙ also, `https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=%5B%3AXID_Continue%3A%5D&abb=on&g=&i=`
-- does not seem to include 'MATHEMATICAL DOUBLE-STRUCK DIGIT ONE'
data 𝟙 : Set where
  ◇ : 𝟙

-- data _𝟙 : Nat → Set where
--   ◇_ : ∀ n → _𝟙 n
