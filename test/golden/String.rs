#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub fn s42() -> String {
  "42".to_string()
}

pub fn main () {
  println!("{}:\t\t\t {}", module_path!(),
    s42()
  );
}
