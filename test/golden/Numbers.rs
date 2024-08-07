#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn answer() -> i32 {
  42
}

pub fn suc(x: i32) -> i32 {
  1 + x
}

pub fn add_answer(x: i32) -> i32 {
  answer() + x
}

pub fn add(x: i32, x0: i32) -> i32 {
  x + x0
}

pub fn add3(x: i32, x0: i32, x1: i32) -> i32 {
  x + x0 + x1
}

pub fn add3b(x: i32, x0: i32, x1: i32) -> i32 {
  apply!(add, x, apply!(add, x0, x1))
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {} | {} | {}", module_path!(),
    answer(),
    suc(41),
    add_answer(0),
    add(40, 2),
    add3(40, 1, 1),
    add3b(40, 1, 1),
  );
}
