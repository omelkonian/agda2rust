#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn c4() -> char {
  '\u{01d7dc}'
}

pub fn c2() -> char {
  '\u{01d7da}'
}

pub fn main () {
  println!("{}:\t\t {}{}", module_path!(),
    c4(), c2()
  );
}
