#![allow(dead_code, non_snake_case, unused_variables)]
fn catchAll<A>() -> A { panic!("CATCH_ALL") }
pub enum Maybe<  A,> {
  Nothing(),
  Just(A),
  CatchAll(std::marker::PhantomData<(A,)>),
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
    _ => catchAll(),
  }
}
pub fn maybeToBool(x0: Maybe<i32>) -> i32 {
  { let x1 = 1; match x0 { Maybe::Nothing() => 0, _ => x1 } }
}
pub fn fromMaybe<  A,>(x0: A, x1: Maybe<A>) -> A {
  match x1 {
    Maybe::Nothing() => x0,
    Maybe::Just(x2) => x2,
    _ => catchAll(),
  }
}
