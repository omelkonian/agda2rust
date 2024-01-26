#![allow(dead_code, non_snake_case, unused_variables)]
fn _impossible<A>() -> A { panic!("IMPOSSIBLE") }
pub enum Bool {
  r#false(),
  r#true(),
  _Impossible(std::marker::PhantomData<()>),
}


