#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn add40(n: i32) -> i32 {
  40 + n
}

pub fn add41(n: i32) -> i32 {
  1 + apply!(add40, n)
}

pub fn testAdd40() -> i32 {
  1 + apply!(add40, 1)
}

pub fn testAdd41() -> i32 {
  apply!(add41, 1)
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {}", module_path!(),
    add40(2),
    add41(1),
    testAdd40(),
    testAdd41(),
  );
}
