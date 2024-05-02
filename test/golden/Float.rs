#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub fn idFloat(x0: f64) -> f64 {
  x0
}

pub fn testFloat() -> f64 {
  4.2
}

pub fn main () {
  println!("{}:\t {}", module_path!(),
    idFloat(testFloat())
  );
}
