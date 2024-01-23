// *** module Test ***
#![allow(dead_code, non_snake_case, unused_variables)]
fn catchAll<A>() -> A { panic!("CATCH_ALL") }


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
  CatchAll(std::marker::PhantomData<(A,)>),
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
    Maybe::Just(x1) => x1,
    _ => catchAll(),
  }
}
fn maybeToBool(x0: Maybe<i32>) -> i32 {
  { let x1 = 1; match x0 { Maybe::Nothing() => 0, _ => x1 } }
}
fn fromMaybe<  A,>(x0: A, x1: Maybe<A>) -> A {
  match x1 {
    Maybe::Nothing() => x0,
    Maybe::Just(x2) => x2,
    _ => catchAll(),
  }
}
enum Either<A, B> {
  Left(A),
  Right(B),
  CatchAll(std::marker::PhantomData<(A, B)>),
}


fn fromEither<A, B>(x0: A, x1: Either<A, B>) -> A {
  match x1 {
    Either::Left(x2) => x2,
    Either::Right(x2) => x0,
    _ => catchAll(),
  }
}
enum EitherL<A, B> {
  Left(A),
  CatchAll(std::marker::PhantomData<(A, B)>),
}

