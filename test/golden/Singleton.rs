#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

#[derive(Debug)]
pub enum The<A> {
  the(A),
}

use self::The::{the};

pub fn main() {
  println!("{}:\t\t {:?}", module_path!(),
    the(42),
  );
}
