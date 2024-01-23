// *** module Agda.Builtin.Nat ***
#![allow(dead_code, non_snake_case, unused_variables)]
fn catchAll<A>() -> A { panic!("CATCH_ALL") }
enum Nat {
  zero(),
  suc(i32),
  CatchAll(std::marker::PhantomData<()>),
}









