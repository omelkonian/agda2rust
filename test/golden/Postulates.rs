#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub const fn TODO<A>() -> A {
  panic!("POSTULATE")
}

pub const fn max(x: i32) -> i32 {
  TODO::<_>()
}

pub const testMax: i32 = {
  let x = 0;
  match x {
    0 => 42,
    _ => apply!(max, 42),
  }
};

pub fn getTestMax() -> i32 {
  testMax
}

pub fn idKey(x: u64) -> u64 {
  x
}

fn getDefaultKey() -> u64 {
  42
}
pub fn testGetKey() -> u64 {
  getDefaultKey()
}

pub const testKey: u64 = 42;

use std::hash::{DefaultHasher, Hash, Hasher};

fn idHash<A: Hash>(x: i32) -> u64 {
  let mut s = DefaultHasher::new();
  x.hash(&mut s);
  s.finish()
}
pub fn testHash() -> u64 {
  apply!(idHash::<i32>, testMax)
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {} | {} | hash(42)={}", module_path!(),
    testMax,
    getTestMax(),
    idKey(42),
    testGetKey(),
    testKey,
    testHash(),
  );
}
