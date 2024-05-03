#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

#[derive(Debug)]
pub enum Unit {
  tt(),
}

pub fn idUnit(x0: Unit) -> Unit {
  Unit::tt()
}

pub fn main () {
  println!("{}:\t {:?}", module_path!(),
    idUnit(Unit::tt())
  );
}
