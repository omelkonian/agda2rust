#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn increment(x: i32) -> i32 {
  1 + x
}

pub fn main() {
  println!("{}:\t {}", module_path!(),
    increment(41)
  );
}
