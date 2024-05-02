#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub struct Σ<A, B> {
  pub fst: A,
  pub snd: B,
}

pub fn Σ·fst<A, B>(r: Σ<A, B>) -> A {
  match r {
    Σ { fst, snd } => fst,
  }
}

pub fn Σ·snd<A, B>(r: Σ<A, B>) -> B {
  match r {
    Σ { fst, snd } => snd,
  }
}

