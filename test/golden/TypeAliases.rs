#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub type ℕ = i32;

pub fn testAlias() -> ℕ {
  42
}

pub type ℕՖ8594Ֆℕ = fn(_: ℕ) -> ℕ;

pub fn incr(x: ℕ) -> ℕ {
  1 + x
}

pub fn testAliasF() -> ℕ {
  incr(41)
}

pub type Id<A> = A;

pub fn id<A>(x: Id<A>) -> Id<A> {
  x
}

pub fn main () {
  println!("{}:\t\t {} | {} | {}", module_path!(),
    testAlias(),
    testAliasF(),
    id(42),
    // idK(42),
  );
}
