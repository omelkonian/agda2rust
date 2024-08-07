#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn tt() -> bool {
  true
}

pub fn not(x: bool) -> bool {
  match x {
    false => true,
    true => false,
  }
}

pub fn _Ֆ8743Ֆ_(x: bool, x0: bool) -> bool {
  {
    let x1 = false;
    match x {
      true => match x0 {
        true => true,
        _ => x1,
      },
      _ => x1,
    }
  }
}

pub fn testBool() -> bool {
  apply!(_Ֆ8743Ֆ_, true, false)
}

pub fn bool2Nat(x: bool) -> i32 {
  match x {
    false => 42,
    true => 0,
  }
}

pub fn if_then_else_<A>(x: bool, x0: A, x1: A) -> A {
  match x {
    false => x1,
    true => x0,
  }
}

pub fn testIte() -> i32 {
  apply!(if_then_else_::<i32>, true, 42, 0)
}

pub fn main () {
  println!("{}:\t\t {} | {}", module_path!(),
    bool2Nat(testBool()),
    testIte(),
  );
}
