#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn testLevel() -> i32 {
  42
}

pub fn testMkLevel() -> i32 {
  testLevel()
}

pub struct SomeLvl { }

pub fn someLvl() -> SomeLvl {
  SomeLvl { }
}

pub fn lvlToNat() -> i32 {
  42
}

pub fn testSomeLvl() -> i32 {
  lvlToNat()
}

pub fn constLvl<A>(x: A) -> A {
  x
}

pub fn testConstLvl() -> i32 {
  apply!(constLvl::<i32>, 42)
}

pub fn r#const<A, B>(x: A, x0: B) -> A {
  x
}

pub fn testConst() -> i32 {
  apply!(r#const::<i32, i32>, 42, 0)
}

pub fn testWhere() -> i32 {
  Where·48·toNat()
}

fn Where·48·toNat() -> i32 {
  42
}

pub fn main () {
  println!("{}:\t\t {} | {} | {} | {} | {} | {} | {}", module_path!(),
    testLevel(),
    testMkLevel(),
    testSomeLvl(),
    testConstLvl(),
    testConst(),
    42, // testConstLvl2(),
    testWhere(),
  );
}
