#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub enum Either<A, B> {
  Left(A),
  Right(B),
}

pub fn fromEitherAB<A, B>(x: A, x0: Either<A, B>) -> A {
  match x0 {
    Either::Left(x1) => x1,
    Either::Right(x1) => x,
  }
}

pub fn fromEither<A, B>(x: A, x0: Either<A, B>) -> A {
  match x0 {
    Either::Left(x1) => x1,
    Either::Right(x1) => x,
  }
}

pub enum OnlyLeft<A, B> {
  Left(A),
  _Impossible(std::marker::PhantomData<(B,)>),
}

pub fn fromOnlyLeft<A, B>(x: OnlyLeft<A, B>) -> A {
  match x {
    OnlyLeft::Left(x0) => x0,
    _ => unreachable!(),
  }
}

pub fn fromOnlyLeft2<A, B, C>(x: OnlyLeft<OnlyLeft<A, B>, C>) -> A {
  match x {
    OnlyLeft::Left(x0) => match x0 {
      OnlyLeft::Left(x1) => x1,
      _ => unreachable!(),
    },
    _ => unreachable!(),
  }
}

pub struct OnlyLeftR<A, B> {
  pub left: A,
  pub _phantom: std::marker::PhantomData<(B,)>,
}

pub fn OnlyLeftR·left<A, B>(r: OnlyLeftR<A, B>) -> A {
  match r {
    OnlyLeftR { left, _phantom } => left,
  }
}

pub fn fromOnlyLeftR<A, B>(x: OnlyLeftR<A, B>) -> A {
  apply!(OnlyLeftR·left, x)
}

pub fn fromOnlyLeftR2<A, B, C>(x: OnlyLeftR<OnlyLeftR<A, B>, C>) -> A {
  apply!(OnlyLeftR·left, apply!(OnlyLeftR·left, x))
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {} | {} | {}", module_path!(),
    fromEitherAB::<i32, i32>(41, Either::Left(42)),
    fromEither::<i32, i32>(41, Either::Left(42)),
    fromOnlyLeft::<i32, i32>(OnlyLeft::Left(42)),
    fromOnlyLeftR::<i32, i32>(OnlyLeftR {left: 42, _phantom: __()}),
    fromOnlyLeft2::<i32, i32, i32>(OnlyLeft::Left(OnlyLeft::Left(42))),
    fromOnlyLeftR2::<i32, i32, i32>(OnlyLeftR
      {left: OnlyLeftR {left: 42, _phantom: __()},
      _phantom: __()}
    ),
  );
}
