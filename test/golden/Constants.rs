#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub enum Maybe<A> {
  nothing(),
  just(A),
}

pub fn naught<A>() -> Maybe<A> {
  Maybe::nothing()
}

pub fn justify<A>(x: Maybe<A>, x0: A) -> A {
  match x {
    Maybe::nothing() => x0,
    Maybe::just(x1) => x1,
  }
}

pub fn testNaught() -> i32 {
  justify::<i32>(naught::<i32>(), 42)
}

pub const the42: i32 = 42;

pub fn main() {
  println!("{}:\t\t {} | {}", module_path!(),
    testNaught(),
    the42
  );
}
