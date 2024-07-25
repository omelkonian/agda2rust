#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

#[derive(Debug,Clone)]
pub enum Newtype<A> {
  mk(A),
}

pub fn k<A>(x: Newtype<A>, x0: Newtype<A>) -> A {
  match x {
    Newtype::mk(x1) => x1,
  }
}

pub fn main() {
  println!("{}:\t\t\t {}", module_path!(),
    k(Newtype::mk(42), Newtype::mk(0)),
  );
}
