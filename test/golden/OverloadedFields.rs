#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

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

pub fn idX(x0: X) -> i32 {
  X·id(x0)
}

pub fn idY(x0: Y) -> i32 {
  Y·id(x0)
}

pub fn main() {
  println!("{}: {} | {} | {} | {} ", module_path!(),
    X·id(X{id: 42}),
    Y·id(Y{id: 42}),
    idX(exX()),
    idY(exY()),
  );
}
