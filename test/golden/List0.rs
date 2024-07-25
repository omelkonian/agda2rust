#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

#[derive(Debug)]
pub enum List<A> {
  Ֆ91ՖՖ93Ֆ(),
  _Ֆ8759Ֆ_(A, Box<List<A>>),
}

pub fn _Ֆ43ՖՖ43Ֆ_<A>(x: List<A>, x0: List<A>) -> List<A> {
  match x {
    List::Ֆ91ՖՖ93Ֆ() => x0,
    List::_Ֆ8759Ֆ_(x1, xs) => {
      let xs = *xs;
      List::_Ֆ8759Ֆ_(x1, Box::new(_Ֆ43ՖՖ43Ֆ_::<A>(xs, x0)))
    },
  }
}

pub fn map<A, B>(x: impl Fn(A) -> B, x0: List<A>) -> List<B> {
  match x0 {
    List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
    List::_Ֆ8759Ֆ_(x1, xs) => {
      let xs = *xs;
      List::_Ֆ8759Ֆ_(x(x1), Box::new(map::<A, B>(x, xs)))
    },
  }
}

pub fn flatten<A>(x: List<List<A>>) -> List<A> {
  match x {
    List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
    List::_Ֆ8759Ֆ_(x0, xs) => {
      let xs = *xs;
      _Ֆ43ՖՖ43Ֆ_::<A>(x0, flatten::<A>(xs))
    },
  }
}

pub fn head<A>(x: List<List<A>>) -> List<A> {
  match x {
    List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
    List::_Ֆ8759Ֆ_(x0, xs) => {
      let xs = *xs;
      match x0 {
        List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
        List::_Ֆ8759Ֆ_(x1, xs0) => {
          let xs0 = *xs0;
          List::_Ֆ8759Ֆ_(x1, Box::new(List::Ֆ91ՖՖ93Ֆ()))
        },
      }
    },
  }
}

pub const empty: List<i32> = List::Ֆ91ՖՖ93Ֆ();

pub fn headNum(x: List<i32>) -> i32 {
  match x {
    List::Ֆ91ՖՖ93Ֆ() => 42,
    List::_Ֆ8759Ֆ_(x0, xs) => { let xs = *xs; x0 },
  }
}

pub fn single(x: i32) -> List<i32> {
  List::_Ֆ8759Ֆ_(x, Box::new(List::Ֆ91ՖՖ93Ֆ()))
}

pub fn sum(x: List<i32>) -> i32 {
  match x {
    List::Ֆ91ՖՖ93Ֆ() => 0,
    List::_Ֆ8759Ֆ_(x0, xs) => { let xs = *xs; sum(xs) + x0 },
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
