// *** module Test ***
#![allow(dead_code, non_snake_case)]


fn answer() -> i32 {
  42
}
fn suc(x0: i32) -> i32 {
  1 + x0
}
fn add_answer(x0: i32) -> i32 {
  answer() + x0
}
fn add(x0: i32, x1: i32) -> i32 {
  x0 + x1
}
fn add3(x0: i32, x1: i32, x2: i32) -> i32 {
  x0 + x1 + x2
}
fn add3b(x0: i32, x1: i32, x2: i32) -> i32 {
  add(x0, add(x1, x2))
}
enum Maybe<  A,> {
  Nothing(),
  Just(A),
}


fn m0() -> Maybe<i32> {
  Maybe::Nothing()
}
fn m1() -> Maybe<i32> {
  Maybe::Just(1)
}
fn fromMaybeNat(x0: Maybe<i32>) -> i32 {
  match x0 {
    Maybe::Nothing() => 0,
    Maybe::Just(x0) => x0,
  }
}
fn maybeToBool(x0: Maybe<i32>) -> i32 {
  { let x1 = 1; match x0 { Maybe::Nothing() => 0, _ => x1 } }
}
