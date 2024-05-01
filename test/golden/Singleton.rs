#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

#[derive(Debug)]
pub enum The<  A,> {
  the(A),
}

use self::The::{the};

pub fn main() {
  println!("{}: {:?}", module_path!(),
    the(42),
  );
}
