#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub fn c4() -> char {
  '\u{01d7dc}'
}

pub fn c2() -> char {
  '\u{01d7da}'
}

pub fn main () {
  println!("{}:\t\t\t {}{}", module_path!(),
    c4(), c2()
  );
}
