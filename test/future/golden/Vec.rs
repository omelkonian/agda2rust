#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
pub enum Nat {
  zero(),
  suc(Box<Nat>),
  _Impossible(std::marker::PhantomData<()>),
}


#[derive(Debug)]
pub enum Vec<  A,> {
  Ֆ91ՖՖ93Ֆ(),
  _Ֆ8759Ֆ_(A, Box<Vec<A>>),
  _Impossible(std::marker::PhantomData<(A,)>),
}


pub fn vectorLength<  A,>(n: Nat, x0: Vec<A>) -> Nat {
  n
}
pub fn nilLength(x0: Nat) -> Nat {
  Nat::zero()
}
