#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

#[derive(Debug)]
pub enum Nat {
  zero(),
  suc(Box<Nat>),
}

pub fn _Ֆ43Ֆ_(x: Nat, x0: Nat) -> Nat {
  match x {
    Nat::zero() => x0,
    Nat::suc(x1) => {
      let x1 = *x1;
      apply!(Nat::suc, ᐁ(apply!(_Ֆ43Ֆ_, x1, x0)))
    },
  }
}

use self::Nat::{zero,suc};

pub fn main() {
  println!("{}:\t\t {:?} | {:?}", module_path!(),
    suc(ᐁ(suc(ᐁ(suc(ᐁ(suc(ᐁ(zero())))))))),
    _Ֆ43Ֆ_(suc(ᐁ(zero())), suc(ᐁ(zero()))),
  );
}
