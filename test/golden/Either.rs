#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub enum Either<A, B> {
  Left(A),
  Right(B),
}

pub fn fromEither<A, B>(x0: A, x1: Either<A, B>) -> A {
  match x1 {
    Either::Left(x2) => x2,
    Either::Right(x2) => x0,
  }
}

pub enum OnlyLeft<A, B> {
  Left(A),
  _Impossible(std::marker::PhantomData<(B,)>),
}

pub fn fromOnlyLeft<A, B>(x0: OnlyLeft<A, B>) -> A {
  match x0 {
    OnlyLeft::Left(x1) => x1,
    _ => panic!("IMPOSSIBLE"),
  }
}

pub struct OnlyLeftR<A, B> {
  pub left: A,
  pub _phantom: std::marker::PhantomData<(B,)>,
}

pub fn left<A, B>(r: OnlyLeftR<A, B>) -> A {
  match r {
    OnlyLeftR { left, _phantom } => left,
  }
}

pub fn fromOnlyLeftR<A, B>(x0: OnlyLeftR<A, B>) -> A {
  left(x0)
}

use std::marker::{PhantomData};
fn __<T>() -> PhantomData<T> { return PhantomData; }

pub fn main() {
  println!("{}: {} | {} | {}", module_path!(),
    fromEither::<i32, i32>(41, Either::Left(42)),
    fromOnlyLeft::<i32, i32>(OnlyLeft::Left(42)),
    fromOnlyLeftR::<i32, i32>(OnlyLeftR {left: 42, _phantom: __()}),
  );
}
