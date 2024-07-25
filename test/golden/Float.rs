#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub fn idFloat(x: f64) -> f64 {
  x
}

pub fn testFloat() -> f64 {
  idFloat(4.2)
}

pub fn main () {
  println!("{}:\t\t\t {}", module_path!(),
    idFloat(testFloat())
  );
}
