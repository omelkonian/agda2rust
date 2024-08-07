#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub enum Maybe<A> {
  nothing(),
  just(A),
}

pub fn naught<A>() -> Maybe<A> {
  Maybe::nothing()
}

pub fn justify<A>(x: Maybe<A>, x0: A) -> A {
  match x {
    Maybe::nothing() => x0,
    Maybe::just(x1) => x1,
  }
}

pub fn testNaught() -> i32 {
  apply!(justify::<i32>, naught::<i32>(), 42)
}

pub const the42: i32 = 42;

pub fn main() {
  println!("{}:\t\t {} | {}", module_path!(),
    testNaught(),
    the42
  );
}
