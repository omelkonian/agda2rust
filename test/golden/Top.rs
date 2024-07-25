#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

#[derive(Debug)]
pub enum Unit {
  tt(),
}

pub fn idUnit(x: Unit) -> Unit {
  Unit::tt()
}

pub fn main () {
  println!("{}:\t\t\t {:?}", module_path!(),
    idUnit(Unit::tt())
  );
}
