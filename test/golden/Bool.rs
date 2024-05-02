#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub fn tt() -> bool {
  true
}

pub fn not(x0: bool) -> bool {
  match x0 {
    false => true,
    true => false,
  }
}

pub fn _Ֆ8743Ֆ_(x0: bool, x1: bool) -> bool {
  {
    let x2 = false;
    match x0 {
      true => match x1 {
        true => true,
        _ => x2,
      },
      _ => x2,
    }
  }
}

pub fn testBool() -> bool {
  _Ֆ8743Ֆ_(true, false)
}

pub fn bool2Nat(x0: bool) -> i32 {
  match x0 {
    false => 42,
    true => 0,
  }
}

pub fn main () {
  println!("{}:\t {}", module_path!(),
    bool2Nat(testBool())
  );
}
