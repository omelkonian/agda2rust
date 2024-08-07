#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn add(x: i32, x0: i32) -> i32 {
  x + x0
}

pub fn add2l(x: i32) -> i32 {
  2 + x
}

pub fn add2r(x: i32) -> i32 {
  2 + x
}

pub fn add3(x: i32, x0: i32, x1: i32) -> i32 {
  x + x0 + x1
}

pub fn maybeAddL(x: bool, x0: i32) -> i32 {
  match x {
    false => 10 + x0,
    true => 40 + x0,
  }
}

pub fn maybeAddLb(x: bool, x0: i32) -> i32 {
  match x {
    false => 10 + x0,
    true => 40 + x0,
  }
}

pub fn maybeAddR(x: bool, x0: i32) -> i32 {
  match x {
    false => 10 + x0,
    true => 40 + x0,
  }
}

pub fn maybeAddRb(x: bool, x0: i32) -> i32 {
  match x {
    false => 10 + x0,
    true => 40 + x0,
  }
}

pub fn maybeAdd(x: bool, x0: i32, x1: i32) -> i32 {
  match x {
    false => x0 * x1,
    true => x0 + x1,
  }
}

pub fn maybeAdd3(x: bool, x0: i32, x1: i32) -> i32 {
  match x {
    false => 10 + x0 + x1,
    true => 40 + x0 + x1,
  }
}

pub fn maybeAdd3b(x: bool, x0: i32, x1: i32) -> i32 {
  match x {
    false => 10 + x0 + x1,
    true => 40 + x0 + x1,
  }
}

pub enum Maybe<A> {
  nothing(),
  just(A),
}

pub fn maybeInc(x: Maybe<i32>, x0: i32) -> i32 {
  match x {
    Maybe::nothing() => x0,
    Maybe::just(x1) => x0 + x1,
  }
}

pub fn maybeAddM(x: Maybe<i32>, x0: i32, x1: i32) -> i32 {
  match x {
    Maybe::nothing() => x0 + x1,
    Maybe::just(x2) => x0 + x1 + x2,
  }
}

pub fn maybeIncInc(x: Maybe<Maybe<i32>>, x0: i32) -> i32 {
  match x {
    Maybe::nothing() => x0,
    Maybe::just(x1) => match x1 {
      Maybe::nothing() => 1 + x0,
      Maybe::just(x2) => x0 + x2,
    },
  }
}

pub fn maybeIncInc2(x: Maybe<Maybe<i32>>, x0: i32, x1: i32) -> i32 {
  match x {
    Maybe::nothing() => x0 + x1,
    Maybe::just(x2) => match x2 {
      Maybe::nothing() => 1 + x0 + x1,
      Maybe::just(x3) => x0 + x1 + x3,
    },
  }
}

pub fn main() {
  println!("{}:\t {} | {} | {} | {} |\
    {} | {} | {} | {} | {} | {} | {} | {} |\
    {} | {} | {} | {}", module_path!(),

    add(40, 2),
    add2l(40),
    add2r(40),
    add3(40, 1, 1),

    maybeAddL(true, 2),
    maybeAddLb(true, 2),
    maybeAddR(true, 2),
    maybeAddRb(true, 2),
    maybeAdd(true, 40, 2),
    maybeAdd(false, 2, 21),
    maybeAdd3(true, 1, 1),
    maybeAdd3b(true, 1, 1),

    maybeInc(Maybe::just(40), 2),
    maybeAddM(Maybe::just(40), 1, 1),
    maybeIncInc(Maybe::just(Maybe::just(40)), 2),
    maybeIncInc2(Maybe::just(Maybe::just(40)), 1, 1),
  );
}
