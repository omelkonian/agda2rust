#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
pub struct _Ֆ215Ֆ_<A, B> {
  pub projՖ8321Ֆ: A,
  pub projՖ8322Ֆ: B,
}

pub fn projՖ8321Ֆ<A, B>(r: _Ֆ215Ֆ_<A, B>) -> A {
  match r {
    _Ֆ215Ֆ_ { projՖ8321Ֆ, projՖ8322Ֆ } => projՖ8321Ֆ,
  }
}
pub fn projՖ8322Ֆ<A, B>(r: _Ֆ215Ֆ_<A, B>) -> B {
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
