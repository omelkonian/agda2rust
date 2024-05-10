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

pub fn flatten<  A,>(x0: List<List<A>>) -> List<A> {
  match x0 {
    List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
    List::_Ֆ8759Ֆ_(x, xs) => {
      let xs = *xs;
      _Ֆ43ՖՖ43Ֆ_::<A>(x, flatten::<A>(xs))
    },
  }
}

pub fn head<  A,>(x0: List<List<A>>) -> List<A> {
  match x0 {
    List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
    List::_Ֆ8759Ֆ_(x, xs) => {
      let xs = *xs;
      match x {
        List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
        List::_Ֆ8759Ֆ_(x, xs) => {
          let xs = *xs;
          List::_Ֆ8759Ֆ_(x, Box::new(List::Ֆ91ՖՖ93Ֆ()))
        },
      }
    },
  }
}

pub const empty: List<i32> = List::Ֆ91ՖՖ93Ֆ();

pub fn headNum(x0: List<i32>) -> i32 {
  match x0 {
    List::Ֆ91ՖՖ93Ֆ() => 42,
    List::_Ֆ8759Ֆ_(x, xs) => { let xs = *xs; x },
  }
}

pub fn single(x0: i32) -> List<i32> {
  List::_Ֆ8759Ֆ_(x0, Box::new(List::Ֆ91ՖՖ93Ֆ()))
}

pub fn sum(x0: List<i32>) -> i32 {
  match x0 {
    List::Ֆ91ՖՖ93Ֆ() => 0,
    List::_Ֆ8759Ֆ_(x, xs) => { let xs = *xs; sum(xs) + x },
  }
}

pub fn testFlatten() -> i32 {
  sum(flatten::<i32>(List::_Ֆ8759Ֆ_(
    List::_Ֆ8759Ֆ_(20, Box::new(List::Ֆ91ՖՖ93Ֆ())),
    Box::new(List::_Ֆ8759Ֆ_(
      List::_Ֆ8759Ֆ_(20, Box::new(List::Ֆ91ՖՖ93Ֆ())),
      Box::new(List::_Ֆ8759Ֆ_(
        List::_Ֆ8759Ֆ_(2, Box::new(List::Ֆ91ՖՖ93Ֆ())),
        Box::new(List::Ֆ91ՖՖ93Ֆ()),
      )),
    )),
  )))
}

pub fn testHead() -> List<i32> {
  head::<i32>(List::_Ֆ8759Ֆ_(
    List::_Ֆ8759Ֆ_(
      42,
      Box::new(List::_Ֆ8759Ֆ_(
        2,
        Box::new(List::_Ֆ8759Ֆ_(3, Box::new(List::Ֆ91ՖՖ93Ֆ()))),
      )),
    ),
    Box::new(List::_Ֆ8759Ֆ_(
      List::Ֆ91ՖՖ93Ֆ(),
      Box::new(List::_Ֆ8759Ֆ_(List::Ֆ91ՖՖ93Ֆ(), Box::new(List::Ֆ91ՖՖ93Ֆ()))),
    )),
  ))
}

fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

use self::List::{Ֆ91ՖՖ93Ֆ,_Ֆ8759Ֆ_};

pub fn main() {
  println!("{}:\t\t\t {:?} | {:?} | {:?} | {} | {:?}", module_path!(),
    headNum(empty),
    single(42),
    map(|x| x + 1, _Ֆ43ՖՖ43Ֆ_(
      _Ֆ8759Ֆ_(3, ᐁ(Ֆ91ՖՖ93Ֆ())),
      _Ֆ8759Ֆ_(1, ᐁ(Ֆ91ՖՖ93Ֆ()))
    )),
    testFlatten(),
    testHead(),
  );
}
