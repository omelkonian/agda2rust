#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub fn constNat(x: i32, x0: i32) -> i32 {
  x
}

pub fn the42() -> i32 {
  constNat(42, 0)
}

pub fn main() {
  println!("{}:\t\t {} | {}", module_path!(),
    constNat(42, 41),
    the42(),
  );
}
