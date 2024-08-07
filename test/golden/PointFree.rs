#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

#[derive(Debug)]
pub enum Foo {
  foo(i32),
}

pub fn f(x: i32) -> Foo {
  apply!(Foo::foo, x)
}

pub fn g(x: i32) -> i32 {
  1 + x
}

pub fn h(x: i32) -> i32 {
  apply!(g, x)
}

pub fn main() {
  println!("{}:\t\t {:?} | {} | {}", module_path!(),
    f(42),
    g(41),
    h(41),
  );
}
