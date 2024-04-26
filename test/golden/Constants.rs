#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub enum Maybe<  A,> {
  nothing(),
  just(A),
}

pub fn naught<  A,>() -> Maybe<A> {
  Maybe::nothing()
}

pub fn justify<  A,>(x0: Maybe<A>, x1: A) -> A {
  match x0 {
    Maybe::nothing() => x1,
    Maybe::just(x2) => x2,
  }
}

pub fn testNaught() -> i32 {
  justify(naught(), 42)
}

