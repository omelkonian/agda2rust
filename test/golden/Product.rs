#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
fn _impossible<A>() -> A { panic!("IMPOSSIBLE") }
pub struct Product<A, B> {
  pub fst: A,
  pub snd: B,
}

pub fn fst<A, B>(r: Product<A, B>) -> A {
  match r {
    Product { fst, snd } => fst,
  }
}
pub fn snd<A, B>(r: Product<A, B>) -> B {
  match r {
    Product { fst, snd } => snd,
  }
}
pub fn mapFst<A, C, B>(x0: fn(_: A) -> C, x1: Product<A, B>) -> Product<C, B> {
  match x1 {
    Product { fst, snd } => Product { fst: x0(fst), snd: snd },
  }
}
pub fn mapSnd<B, C, A>(x0: fn(_: B) -> C, x1: Product<A, B>) -> Product<A, C> {
  match x1 {
    Product { fst, snd } => Product { fst: fst, snd: x0(snd) },
  }
}
