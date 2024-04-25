#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]
#[derive(Debug)]
pub enum Nat {
  zero(),
  suc(Box<Nat>),
}


pub fn _Ֆ43Ֆ_(x0: Nat, x1: Nat) -> Nat {
  match x0 {
    Nat::zero() => x1,
    Nat::suc(x2) => { let x2 = *x2; Nat::suc(Box::new(_Ֆ43Ֆ_(x2, x1))) },
  }
}
