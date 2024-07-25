#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub const fn TODO<A>() -> A {
  panic!("POSTULATE")
}

pub const fn max(x: i32) -> i32 {
  TODO()
}

pub const testMax: i32 = { let x = 0; match x { 0 => 42, _ => max(42) } };

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
  idHash::<i32>(testMax)
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
