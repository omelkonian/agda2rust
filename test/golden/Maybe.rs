#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

#[derive(Debug)]
pub enum Maybe<A> {
  Nothing(),
  Just(A),
}

pub fn idMaybe<A>(x: Maybe<A>) -> Maybe<A> {
  x
}

pub fn m0() -> Maybe<i32> {
  Maybe::Nothing()
}

pub fn m1() -> Maybe<i32> {
  Maybe::Just(42)
}

pub fn fromMaybeNat(x: Maybe<i32>) -> i32 {
  match x {
    Maybe::Nothing() => 42,
    Maybe::Just(x0) => x0,
  }
}

pub fn fromMaybe<A>(x: A, x0: Maybe<A>) -> A {
  match x0 {
    Maybe::Nothing() => x,
    Maybe::Just(x1) => x1,
  }
}

use self::Maybe::{Nothing,Just};

pub fn main () {
  println!("{}:\t\t\t {:?} | {:?} | {} | {} | {} | {} | {}", module_path!(),
    idMaybe(Just(42)), idMaybe(m1()),
    fromMaybeNat(Nothing()), fromMaybeNat(m0()), fromMaybeNat(m1()),
    fromMaybe(42, m0()), fromMaybe(0, m1()),
  );
}
