#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub enum II {
  Ֆ9671Ֆ(),
  Ֆ9670Ֆ(),
}

pub fn ii() -> II {
  II::Ֆ9671Ֆ()
}

pub fn iiՖ39Ֆ() -> II {
  II::Ֆ9670Ֆ()
}

pub fn IIՖ8594ՖII(x: II) -> II {
  match x {
    II::Ֆ9671Ֆ() => II::Ֆ9670Ֆ(),
    II::Ֆ9670Ֆ() => II::Ֆ9671Ֆ(),
  }
}

pub fn IIՖ8594ՖIIՖ39Ֆ(x: II) -> II {
  match x {
    II::Ֆ9671Ֆ() => II::Ֆ9670Ֆ(),
    II::Ֆ9670Ֆ() => II::Ֆ9671Ֆ(),
  }
}

pub fn IIՖ8594ՖIIՖ39ՖՖ39Ֆ(x: II) -> II {
  match x {
    II::Ֆ9671Ֆ() => II::Ֆ9670Ֆ(),
    II::Ֆ9670Ֆ() => II::Ֆ9671Ֆ(),
  }
}

pub fn IIՖ8594ՖIIՖ39ՖՖ39ՖՖ39Ֆ(x: II) -> II {
  {
    let x0 = apply!(IIՖ8594ՖII, x);
    match x0 {
      II::Ֆ9671Ֆ() => II::Ֆ9670Ֆ(),
      II::Ֆ9670Ֆ() => II::Ֆ9671Ֆ(),
    }
  }
}

pub fn testII() -> i32 {
  {
    let x = II::Ֆ9671Ֆ();
    { let x0 = apply!(IIՖ8594ՖII, x); match x0 { II::Ֆ9670Ֆ() => 42, _ => 0 } }
  }
}

pub enum I {
  Ֆ9671Ֆ(),
}

pub fn i() -> I {
  I::Ֆ9671Ֆ()
}

pub fn IՖ8594ՖII(x: I) -> II {
  II::Ֆ9671Ֆ()
}

pub fn IIՖ8594ՖI(x: II) -> I {
  I::Ֆ9671Ֆ()
}

pub fn testI() -> i32 {
  42
}

pub fn f() -> i32 {
  apply!(Where·74·go, 21)
}

fn Where·74·go(x: i32) -> i32 {
  match x {
    0 => 0,
    _ => { let x0 = x - 1; 2 + apply!(Where·74·go, x0) },
  }
}

pub fn g(x: i32) -> i32 {
  apply!(Where·84·go, x)
}

fn Where·84·go(x: i32) -> i32 {
  match x {
    0 => 0,
    _ => { let x0 = x - 1; 2 + apply!(Where·84·go, x0) },
  }
}

pub fn h(x: i32) -> i32 {
  apply!(Where·94·go, x)
}

fn Where·94·go(x: i32) -> i32 {
  42
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {} | {}", module_path!(),
    testII(),
    testI(),
    f(),
    g(21),
    h(0),
  );
}
