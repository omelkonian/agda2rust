#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
fn _impossible<A>() -> A { panic!("IMPOSSIBLE") }
pub enum Bool {
  r#false(),
  r#true(),
  _Impossible(std::marker::PhantomData<()>),
}


