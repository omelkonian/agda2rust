// *** module Agda.Builtin.List ***
#![allow(dead_code, non_snake_case, unused_variables)]
fn catchAll<A>() -> A { panic!("CATCH_ALL") }
enum List<  A,> {
  nil(),
  cons(A, List<A>),
  CatchAll(std::marker::PhantomData<(A,)>),
}


