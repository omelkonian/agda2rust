#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub struct X {
  pub id: i32,
}

pub fn X·id(r: X) -> i32 {
  match r {
    X { id } => id,
  }
}

pub fn exX() -> X {
  X { id: 42 }
}

pub struct Y {
  pub id: i32,
}

pub fn Y·id(r: Y) -> i32 {
  match r {
    Y { id } => id,
  }
}

pub fn exY() -> Y {
  Y { id: 42 }
}

pub fn idX(x: X) -> i32 {
  X·id(x)
}

pub fn idY(x: Y) -> i32 {
  Y·id(x)
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {} ", module_path!(),
    X·id(X{id: 42}),
    Y·id(Y{id: 42}),
    idX(exX()),
    idY(exY()),
  );
}
