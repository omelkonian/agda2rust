#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
fn _impossible<A>() -> A { panic!("IMPOSSIBLE") }
#[derive(Debug)]
pub enum List<  A,> {
  Nil(),
  Cons(A, Box<List<A>>),
  _Impossible(std::marker::PhantomData<(A,)>),
}


pub fn con<  A,>(x0: List<A>, x1: List<A>) -> List<A> {
  match x0 {
    List::Nil() => x1,
    List::Cons(x, xs) => { let xs = *xs; List::Cons(x, Box::new(con(xs, x1))) },
    _ => _impossible(),
  }
}
