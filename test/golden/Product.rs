#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

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

pub fn mapFst<A, C, B>(
  x: Rc<dyn Fn(A) -> C>,
  x0: _Ֆ215Ֆ_<A, B>,
) -> _Ֆ215Ֆ_<C, B> {
  match x0 {
    _Ֆ215Ֆ_ { projՖ8321Ֆ, projՖ8322Ֆ } => _Ֆ215Ֆ_ {
      projՖ8321Ֆ: apply!(x, projՖ8321Ֆ),
      projՖ8322Ֆ: projՖ8322Ֆ,
    },
  }
}

pub fn mapSnd<B, C, A>(
  x: Rc<dyn Fn(B) -> C>,
  x0: _Ֆ215Ֆ_<A, B>,
) -> _Ֆ215Ֆ_<A, C> {
  match x0 {
    _Ֆ215Ֆ_ { projՖ8321Ֆ, projՖ8322Ֆ } => _Ֆ215Ֆ_ {
      projՖ8321Ֆ: projՖ8321Ֆ,
      projՖ8322Ֆ: apply!(x, projՖ8322Ֆ),
    },
  }
}

pub fn fst<A, B>(x: _Ֆ215Ֆ_<A, B>) -> A {
  apply!(_Ֆ215Ֆ_·projՖ8321Ֆ, x)
}

pub fn main() {
  println!("{}:\t\t {} | {} | {}", module_path!(),
    mapSnd(
      ᐁF(|x| x + 1),
      _Ֆ215Ֆ_ {projՖ8321Ֆ: 0, projՖ8322Ֆ: 41}
    ).projՖ8322Ֆ,
    _Ֆ215Ֆ_·projՖ8321Ֆ(mapFst(
      ᐁF(|x| x + 1),
      _Ֆ215Ֆ_ {projՖ8321Ֆ: 41, projՖ8322Ֆ: 40}
    )),
    fst(mapFst(
      ᐁF(|x| x + 1),
      _Ֆ215Ֆ_ {projՖ8321Ֆ: 41, projՖ8322Ֆ: 40}
    )),
);
}
