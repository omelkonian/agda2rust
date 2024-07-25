#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

#[path = "Agda/Builtin/List.rs"] mod ListMod;
use self::ListMod::List;
pub fn sum(x: List<i32>) -> i32 {
  match x {
    List::Ֆ91ՖՖ93Ֆ() => 0,
    List::_Ֆ8759Ֆ_(x0, xs) => { let xs = *xs; sum(xs) + x0 },
  }
}

pub fn testSum() -> i32 {
  sum(List::_Ֆ8759Ֆ_(
    30,
    Box::new(List::_Ֆ8759Ֆ_(12, Box::new(List::Ֆ91ՖՖ93Ֆ()))),
  ))
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

pub fn zipWith<A, B, C>(
  x: impl Fn(A, B) -> C,
  x0: List<A>,
  x1: List<B>,
) -> List<C> {
  {
    let x2 = List::Ֆ91ՖՖ93Ֆ();
    match x0 {
      List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
      List::_Ֆ8759Ֆ_(x3, xs) => {
        let xs = *xs;
        match x1 {
          List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
          List::_Ֆ8759Ֆ_(x4, xs0) => {
            let xs0 = *xs0;
            List::_Ֆ8759Ֆ_(x(x3, x4), Box::new(zipWith::<A, B, C>(x, xs, xs0)))
          },
        }
      },
      _ => x2,
    }
  }
}

fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

use self::List::{Ֆ91ՖՖ93Ֆ,_Ֆ8759Ֆ_};

pub fn main() {
  println!("{}:\t\t\t {} | {} | {}", module_path!(),
    testSum(),
    sum(map(|x| x + 1, _Ֆ43ՖՖ43Ֆ_(
     _Ֆ8759Ֆ_(39, ᐁ(Ֆ91ՖՖ93Ֆ())),
     _Ֆ8759Ֆ_(1, ᐁ(Ֆ91ՖՖ93Ֆ()))
    ))),
    sum(zipWith(|x, y| x + y,
     _Ֆ8759Ֆ_(40, ᐁ(Ֆ91ՖՖ93Ֆ())),
     _Ֆ8759Ֆ_(2, ᐁ(Ֆ91ՖՖ93Ֆ()))
    )),
  );
}
