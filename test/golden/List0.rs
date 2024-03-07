#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
#[derive(Debug)]
pub enum List<  A,> {
  Ֆ91ՖՖ93Ֆ(),
  _Ֆ8759Ֆ_(A, Box<List<A>>),
}


pub fn _Ֆ43ՖՖ43Ֆ_<  A,>(x0: List<A>, x1: List<A>) -> List<A> {
  match x0 {
    List::Ֆ91ՖՖ93Ֆ() => x1,
    List::_Ֆ8759Ֆ_(x, xs) => {
      let xs = *xs;
      List::_Ֆ8759Ֆ_(x, Box::new(_Ֆ43ՖՖ43Ֆ_(xs, x1)))
    },
  }
}
pub fn map<A, B>(x0: fn(_: A) -> B, x1: List<A>) -> List<B> {
  match x1 {
    List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
    List::_Ֆ8759Ֆ_(x, xs) => {
      let xs = *xs;
      List::_Ֆ8759Ֆ_(x0(x), Box::new(map(x0, xs)))
    },
  }
}
