#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub fn increment(x0: i32) -> i32 {
  1 + x0
}

pub fn main() {
  println!("{}:\t\t {}", module_path!(),
    increment(41)
  );
}
