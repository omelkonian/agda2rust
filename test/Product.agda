record _×_ (A B : Set) : Set where
  constructor _,_
  field fst : A
        snd : B
open _×_ public

private variable A B C : Set

mapFst : (A → C) → A × B → C × B
mapFst f (a , b) = f a , b

mapSnd : (B → C) → A × B → A × C
mapSnd f (a , b) = a , f b
