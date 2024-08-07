#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn primFloatNumericalEquality(x: f64, x0: f64) -> bool {
  primFloatEquality()
}

pub fn primFloatNumericalLess(x: f64, x0: f64) -> bool {
  primFloatLess()
}

pub fn primRound(x: f64) -> Maybe<i32> {
  primFloatRound()
}

pub fn primFloor(x: f64) -> Maybe<i32> {
  primFloatFloor()
}

pub fn primCeiling(x: f64) -> Maybe<i32> {
  primFloatCeiling()
}

pub fn primExp(x: f64) -> f64 {
  primFloatExp()
}

pub fn primLog(x: f64) -> f64 {
  primFloatLog()
}

pub fn primSin(x: f64) -> f64 {
  primFloatSin()
}

pub fn primCos(x: f64) -> f64 {
  primFloatCos()
}

pub fn primTan(x: f64) -> f64 {
  primFloatTan()
}

pub fn primASin(x: f64) -> f64 {
  primFloatASin()
}

pub fn primACos(x: f64) -> f64 {
  primFloatACos()
}

pub fn primATan(x: f64) -> f64 {
  primFloatATan()
}

pub fn primATan2(x: f64, x0: f64) -> f64 {
  primFloatATan2()
}

