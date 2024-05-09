#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub struct _Ֆ215Ֆ_<A, B> {
  pub projՖ8321Ֆ: A,
  pub projՖ8322Ֆ: B,
}

pub fn _Ֆ215Ֆ_·projՖ8321Ֆ<A, B>(r: _Ֆ215Ֆ_<A, B>) -> A {
  match r {
    _Ֆ215Ֆ_ { projՖ8321Ֆ, projՖ8322Ֆ } => projՖ8321Ֆ,
  }
}

pub fn _Ֆ215Ֆ_·projՖ8322Ֆ<A, B>(r: _Ֆ215Ֆ_<A, B>) -> B {
  match r {
    _Ֆ215Ֆ_ { projՖ8321Ֆ, projՖ8322Ֆ } => projՖ8322Ֆ,
  }
}

pub fn mapFst<A, C, B>(x0: fn(_: A) -> C, x1: _Ֆ215Ֆ_<A, B>) -> _Ֆ215Ֆ_<C, B> {
  match x1 {
    _Ֆ215Ֆ_ { projՖ8321Ֆ, projՖ8322Ֆ } => _Ֆ215Ֆ_ {
      projՖ8321Ֆ: x0(projՖ8321Ֆ),
      projՖ8322Ֆ: projՖ8322Ֆ,
    },
  }
}

pub fn mapSnd<B, C, A>(x0: fn(_: B) -> C, x1: _Ֆ215Ֆ_<A, B>) -> _Ֆ215Ֆ_<A, C> {
  match x1 {
    _Ֆ215Ֆ_ { projՖ8321Ֆ, projՖ8322Ֆ } => _Ֆ215Ֆ_ {
      projՖ8321Ֆ: projՖ8321Ֆ,
      projՖ8322Ֆ: x0(projՖ8322Ֆ),
    },
  }
}

pub fn fst<A, B>(x0: _Ֆ215Ֆ_<A, B>) -> A {
  _Ֆ215Ֆ_·projՖ8321Ֆ(x0)
}

pub fn main() {
  println!("{}:\t\t\t {} | {} | {}", module_path!(),
    mapSnd(
      |x| x + 1
      , _Ֆ215Ֆ_ {projՖ8321Ֆ: 0, projՖ8322Ֆ: 41}
    ).projՖ8322Ֆ,
    _Ֆ215Ֆ_·projՖ8321Ֆ(mapFst(
      |x| x + 1
      , _Ֆ215Ֆ_ {projՖ8321Ֆ: 41, projՖ8322Ֆ: 40}
    )),
    fst(mapFst(
      |x| x + 1
      , _Ֆ215Ֆ_ {projՖ8321Ֆ: 41, projՖ8322Ֆ: 40}
    )),
);
}
