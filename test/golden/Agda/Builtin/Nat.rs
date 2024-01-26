#![allow(dead_code, non_snake_case, unused_variables)]
fn _impossible<A>() -> A { panic!("IMPOSSIBLE") }
pub enum Nat {
  zero(),
  suc(i32),
  _Impossible(std::marker::PhantomData<()>),
}









