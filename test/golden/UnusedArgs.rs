#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub fn constNat(x0: i32, x1: i32) -> i32 {
  x0
}

pub fn the42() -> i32 {
  constNat(42, 0)
}

pub fn main() {
  println!("{}: {} | {}", module_path!(),
    constNat(42, 41),
    the42(),
  );
}
