#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub fn add40(n: i32) -> i32 {
  40 + n
}

pub fn add41(n: i32) -> i32 {
  1 + add40(n)
}

pub fn testAdd40() -> i32 {
  1 + add40(1)
}

pub fn testAdd41() -> i32 {
  add41(1)
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {}", module_path!(),
    add40(2),
    add41(1),
    testAdd40(),
    testAdd41(),
  );
}
