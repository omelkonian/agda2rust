#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

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

