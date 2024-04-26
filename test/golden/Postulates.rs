#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub fn TODO<  A,>() -> A {
  panic!("POSTULATE")
}

pub fn max(x0: i32) -> i32 {
  TODO()
}

pub fn testMax() -> i32 {
  { let x0 = 0; match x0 { 0 => 42, _ => max(42) } }
}

