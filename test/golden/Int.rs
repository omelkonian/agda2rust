#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub fn pos42() -> i32 {
  42
}

pub fn neg42() -> i32 {
  -42
}

pub fn main () {
  println!("{}:\t\t\t {} | {}", module_path!(),
    pos42(),
    neg42(),
  );
}
