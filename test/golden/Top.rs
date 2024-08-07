#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

#[derive(Debug)]
pub enum Unit {
  tt(),
}

pub fn idUnit(x: Unit) -> Unit {
  Unit::tt()
}

pub fn main () {
  println!("{}:\t\t\t {:?}", module_path!(),
    idUnit(Unit::tt())
  );
}
