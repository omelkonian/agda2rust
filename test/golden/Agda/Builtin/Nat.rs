#![allow(dead_code, non_snake_case, unused_variables)]
fn catchAll<A>() -> A { panic!("CATCH_ALL") }
pub enum Nat {
  zero(),
  suc(i32),
  CatchAll(std::marker::PhantomData<()>),
}









