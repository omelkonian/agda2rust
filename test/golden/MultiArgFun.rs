#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn exB(x: bool, x0: i32, x1: i32) -> i32 {
  match x {
    false => x0 * x1,
    true => x0 + x1,
  }
}

pub fn exB2(x0: bool, x1: i32, x2: i32) -> i32 {
  match x0 {
    false => x1 * x2,
    true => x1 + x2,
  }
}

pub fn exB3(x: bool, x1: i32, x2: i32) -> i32 {
  match x {
    false => x1 * x2,
    true => x1 + x2,
  }
}

pub fn exB4(x: bool, x0: i32, x2: i32) -> i32 {
  match x {
    false => x0 * x2,
    true => x0 + x2,
  }
}

pub fn exB5(x: bool, x0: i32, x1: i32) -> i32 {
  match x {
    false => x0 * x1,
    true => x0 + x1,
  }
}

pub fn exH2(x0: bool, x1: i32, x2: i32) -> i32 {
  match x0 {
    false => x1 * x2,
    true => x1 + x2,
  }
}

pub fn exH3(x: bool, x1: i32, x2: i32) -> i32 {
  match x {
    false => x1 * x2,
    true => x1 + x2,
  }
}

pub fn exH4(x: bool, x0: i32, x2: i32) -> i32 {
  match x {
    false => x0 * x2,
    true => x0 + x2,
  }
}

pub fn exH5(x: bool, x0: i32, x1: i32) -> i32 {
  match x {
    false => x0 * x1,
    true => x0 + x1,
  }
}

pub fn exF(x: i32, x0: i32) -> i32 {
  x + x0
}

pub fn exG(x: i32, x0: i32) -> i32 {
  x + x0
}

#[path = "Agda/Builtin/Maybe.rs"] mod MaybeMod;
use self::MaybeMod::Maybe;
pub fn exM(x: Maybe<i32>, x0: i32, x1: i32) -> i32 {
  match x {
    Maybe::just(x2) => x0 + x1,
    Maybe::nothing() => x0 * x1,
  }
}

pub fn addN(x: i32, x0: i32) -> i32 {
  x + x0
}

pub fn apply2(
  x: Rc<dyn Fn(i32) -> Rc<dyn Fn(i32) -> i32>>,
  x0: i32,
  x1: i32,
) -> i32 {
  apply!(x, x0, x1)
}

pub fn x() -> i32 {
  apply!(apply2, ᐁF(move|x|ᐁF(move|x0|x+x0)), 40, 2)
}

pub fn y() -> i32 {
  apply!(apply2, ᐁF(move|x|ᐁF(move|x0|apply!(exF, x, x0))), 40, 2)
}

pub fn z() -> i32 {
  apply!(apply2, ᐁF(move|x|ᐁF(move|x0|x+x0)), 40, 2)
}

pub fn w() -> i32 {
  apply!(apply2, ᐁF(move|x|ᐁF(move|x0|apply!(exG, x, x0))), 40, 2)
}

pub fn q() -> i32 {
  apply!(addN, 40, 2)
}

pub fn r() -> i32 {
  apply!(exG, 40, 2)
}

pub fn main() {
  println!("{}:\t\t \
    {} | {} | {} | {} | {} | \
    {} | {} | {} | {} | \
    {} | {} | {} | {} | {} | {}", module_path!(),
    exB(true, 40, 2),
    exB2(true, 40, 2),
    exB3(true, 40, 2),
    exB4(true, 40, 2),
    exB5(true, 40, 2),

    exH2(true, 40, 2),
    exH3(true, 40, 2),
    exH4(true, 40, 2),
    exH5(true, 40, 2),

    x(),
    y(),
    z(),
    w(),
    q(),
    r(),
  );
}
