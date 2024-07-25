#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

#[derive(Debug)]
pub enum Nat {
  zero(),
  suc(Box<Nat>),
}

pub fn _Ֆ43Ֆ_(x: Nat, x0: Nat) -> Nat {
  match x {
    Nat::zero() => x0,
    Nat::suc(x1) => { let x1 = *x1; Nat::suc(Box::new(_Ֆ43Ֆ_(x1, x0))) },
  }
}

fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

use self::Nat::{zero,suc};

pub fn main() {
  println!("{}:\t\t\t {:?} | {:?}", module_path!(),
    suc(ᐁ(suc(ᐁ(suc(ᐁ(suc(ᐁ(zero())))))))),
    _Ֆ43Ֆ_(suc(ᐁ(zero())), suc(ᐁ(zero()))),
  );
}
