#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

#[derive(Debug)]
pub enum Maybe<  A,> {
  Nothing(),
  Just(A),
}

pub fn idMaybe<  A,>(x0: Maybe<A>) -> Maybe<A> {
  x0
}

pub fn m0() -> Maybe<i32> {
  Maybe::Nothing()
}

pub fn m1() -> Maybe<i32> {
  Maybe::Just(42)
}

pub fn fromMaybeNat(x0: Maybe<i32>) -> i32 {
  match x0 {
    Maybe::Nothing() => 42,
    Maybe::Just(x1) => x1,
  }
}

pub fn fromMaybe<  A,>(x0: A, x1: Maybe<A>) -> A {
  match x1 {
    Maybe::Nothing() => x0,
    Maybe::Just(x2) => x2,
  }
}

use self::Maybe::{Nothing,Just};

pub fn main () {
  println!("{}: {:?} | {:?} | {} | {} | {} | {} | {}", module_path!(),
    idMaybe(Just(42)), idMaybe(m1()),
    fromMaybeNat(Nothing()), fromMaybeNat(m0()), fromMaybeNat(m1()),
    fromMaybe(42, m0()), fromMaybe(0, m1()),
  );
}
