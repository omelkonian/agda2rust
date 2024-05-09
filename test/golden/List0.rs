#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

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
      List::_Ֆ8759Ֆ_(x, Box::new(_Ֆ43ՖՖ43Ֆ_::<A>(xs, x1)))
    },
  }
}

pub fn map<A, B>(x0: fn(_: A) -> B, x1: List<A>) -> List<B> {
  match x1 {
    List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
    List::_Ֆ8759Ֆ_(x, xs) => {
      let xs = *xs;
      List::_Ֆ8759Ֆ_(x0(x), Box::new(map::<A, B>(x0, xs)))
    },
  }
}

fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

use self::List::{Ֆ91ՖՖ93Ֆ,_Ֆ8759Ֆ_};

pub fn main() {
  println!("{}:\t\t\t {:?}", module_path!(),
    map(|x| x + 1, _Ֆ43ՖՖ43Ֆ_(
      _Ֆ8759Ֆ_(3, ᐁ(Ֆ91ՖՖ93Ֆ())),
      _Ֆ8759Ֆ_(1, ᐁ(Ֆ91ՖՖ93Ֆ()))
    )),
  );
}
