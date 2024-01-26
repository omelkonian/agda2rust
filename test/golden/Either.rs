#![allow(dead_code, non_snake_case, unused_variables)]
fn catchAll<A>() -> A { panic!("CATCH_ALL") }
pub enum Either<A, B> {
  Left(A),
  Right(B),
  CatchAll(std::marker::PhantomData<(A, B)>),
}


pub fn fromEither<A, B>(x0: A, x1: Either<A, B>) -> A {
  match x1 {
    Either::Left(x2) => x2,
    Either::Right(x2) => x0,
    _ => catchAll(),
  }
}
pub enum EitherL<A, B> {
  Left(A),
  CatchAll(std::marker::PhantomData<(A, B)>),
}

