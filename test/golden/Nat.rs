#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub fn _Ֆ43Ֆ_(x: i32, x0: i32) -> i32 {
  match x {
    0 => x0,
    _ => { let x1 = x - 1; 1 + _Ֆ43Ֆ_(x1, x0) },
  }
}

pub fn testNat() -> i32 {
  _Ֆ43Ֆ_(40, 2)
}

pub fn main () {
  println!("{}:\t\t\t {}", module_path!(),
    testNat()
  );
}
