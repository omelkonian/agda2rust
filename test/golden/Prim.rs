#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub fn testLevel() -> i32 {
  42
}

pub fn main () {
  println!("{}:\t\t\t {}", module_path!(),
    testLevel()
  );
}
