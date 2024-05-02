#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub fn _Ֆ43Ֆ_(x0: i32, x1: i32) -> i32 {
  match x0 {
    0 => x1,
    _ => { let x2 = x0 - 1; 1 + _Ֆ43Ֆ_(x2, x1) },
  }
}

pub fn testNat() -> i32 {
  _Ֆ43Ֆ_(40, 2)
}

pub fn main () {
  println!("{}:\t {}", module_path!(),
    testNat()
  );
}
