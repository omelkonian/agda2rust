#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub fn answer() -> i32 {
  42
}

pub fn suc(x0: i32) -> i32 {
  1 + x0
}

pub fn add_answer(x0: i32) -> i32 {
  answer() + x0
}

pub fn add(x0: i32, x1: i32) -> i32 {
  x0 + x1
}

pub fn add3(x0: i32, x1: i32, x2: i32) -> i32 {
  x0 + x1 + x2
}

pub fn add3b(x0: i32, x1: i32, x2: i32) -> i32 {
  add(x0, add(x1, x2))
}

pub fn main() {
  println!("{}:\t\t\t {} | {} | {} | {} | {} | {}", module_path!(),
    answer(),
    suc(41),
    add_answer(0),
    add(40, 2),
    add3(40, 1, 1),
    add3b(40, 1, 1)
  );
}
