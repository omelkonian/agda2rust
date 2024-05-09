record _×_ (A B : Set) : Set where
  constructor _,_
  field proj₁ : A
        proj₂ : B
open _×_ public

private variable A B C : Set

mapFst : (A → C) → A × B → C × B
mapFst f (a , b) = f a , b

mapSnd : (B → C) → A × B → A × C
mapSnd f (a , b) = a , f b

fst : A × B → A
fst = proj₁

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t\t {} | {} | {}", module_path!(),
    mapSnd(
      |x| x + 1
      , _Ֆ215Ֆ_ {projՖ8321Ֆ: 0, projՖ8322Ֆ: 41}
    ).projՖ8322Ֆ,
    _Ֆ215Ֆ_·projՖ8321Ֆ(mapFst(
      |x| x + 1
      , _Ֆ215Ֆ_ {projՖ8321Ֆ: 41, projՖ8322Ֆ: 40}
    )),
    fst(mapFst(
      |x| x + 1
      , _Ֆ215Ֆ_ {projՖ8321Ֆ: 41, projՖ8322Ֆ: 40}
    )),
);
}
#-}
