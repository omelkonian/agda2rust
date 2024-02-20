#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
pub enum Maybe<  A,> {
  Nothing(),
  Just(A),
  _Impossible(std::marker::PhantomData<(A,)>),
}


pub fn m0() -> Maybe<i32> {
  Maybe::Nothing()
}
pub fn m1() -> Maybe<i32> {
  Maybe::Just(1)
}
pub fn fromMaybeNat(x0: Maybe<i32>) -> i32 {
  match x0 {
    Maybe::Nothing() => 0,
    Maybe::Just(x1) => x1,
    _ => panic!("IMPOSSIBLE"),
  }
}
pub fn maybeToBool(x0: Maybe<i32>) -> i32 {
  { let x1 = 1; match x0 { Maybe::Nothing() => 0, _ => x1 } }
}
pub fn fromMaybe<  A,>(x0: A, x1: Maybe<A>) -> A {
  match x1 {
    Maybe::Nothing() => x0,
    Maybe::Just(x2) => x2,
    _ => panic!("IMPOSSIBLE"),
  }
}
