#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
#[derive(Debug)]
pub enum Nat {
  zero(),
  suc(Box<Nat<>>),
  _Impossible(std::marker::PhantomData<()>),
}


pub fn _Ֆ43Ֆ_(x0: Nat<>, x1: Nat<>) -> Nat<> {
  match x0 {
    Nat::zero() => x1,
    Nat::suc(x2) => { let x2 = *x2; Nat::suc(Box::new(_Ֆ43Ֆ_(x2, x1))) },
    _ => panic!("IMPOSSIBLE"),
  }
}
