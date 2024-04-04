#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
pub enum _Ֆ8801Ֆ_<  A,> {
  refl(),
  _Impossible(std::marker::PhantomData<(A,)>),
}

pub fn cong<A, B>(
  a: A,
  b: A,
  f: fn(_: A) -> B,
  x0: _Ֆ8801Ֆ_<A>,
) -> _Ֆ8801Ֆ_<B> {
  panic!("ERASED")
}
pub fn sym<  A,>(a: A, b: A, x0: _Ֆ8801Ֆ_<A>) -> _Ֆ8801Ֆ_<A> {
  panic!("ERASED")
}
