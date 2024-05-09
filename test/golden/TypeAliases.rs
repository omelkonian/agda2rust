#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub type ℕ = i32;

pub fn testAlias() -> ℕ {
  42
}

pub type ℕՖ8594Ֆℕ = fn(_: ℕ) -> ℕ;

pub fn incr(x0: ℕ) -> ℕ {
  1 + x0
}

pub fn testAliasF() -> ℕ {
  incr(41)
}

pub type Id<  A,> = A;

pub fn id<  A,>(x0: Id<A>) -> Id<A> {
  x0
}

pub fn main () {
  println!("{}:\t\t {} | {} | {}", module_path!(),
    testAlias(),
    testAliasF(),
    id(42),
  );
}
