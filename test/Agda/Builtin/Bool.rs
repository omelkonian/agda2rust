// *** module Agda.Builtin.Bool ***
#![allow(dead_code, non_snake_case, unused_variables)]
fn catchAll<A>() -> A { panic!("CATCH_ALL") }
enum Bool {
  r#false(),
  r#true(),
  CatchAll(std::marker::PhantomData<()>),
}


