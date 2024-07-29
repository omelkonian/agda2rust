#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

#[path = "Agda/Builtin/List.rs"] mod ListMod;
use self::ListMod::List;
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

pub fn incr(x: List<i32>) -> List<i32> {
  map::<i32, i32>(|x0| 1 + x0, x)
}

pub fn incr2(x: List<i32>) -> List<i32> {
  map::<i32, i32>(|x0| 1 + x0, x)
}

pub fn sum(x: List<i32>) -> i32 {
  match x {
    List::Ֆ91ՖՖ93Ֆ() => 0,
    List::_Ֆ8759Ֆ_(x0, xs) => { let xs = *xs; sum(xs) + x0 },
  }
}

pub fn testPartialAdd() -> i32 {
  sum(map::<i32, i32>(
    |x| 1 + x,
    List::_Ֆ8759Ֆ_(
      20,
      Box::new(List::_Ֆ8759Ֆ_(20, Box::new(List::Ֆ91ՖՖ93Ֆ()))),
    ),
  ))
}

pub fn testIncr() -> i32 {
  sum(incr(List::_Ֆ8759Ֆ_(
    20,
    Box::new(List::_Ֆ8759Ֆ_(20, Box::new(List::Ֆ91ՖՖ93Ֆ()))),
  )))
}

pub fn testIncr2() -> i32 {
  sum(incr2(List::_Ֆ8759Ֆ_(
    20,
    Box::new(List::_Ֆ8759Ֆ_(20, Box::new(List::Ֆ91ՖՖ93Ֆ()))),
  )))
}

pub fn testPartialAdd2() -> i32 {
  sum(zipWith::<i32, i32, i32>(
    |x, x0| x + x0,
    List::_Ֆ8759Ֆ_(
      20,
      Box::new(List::_Ֆ8759Ֆ_(20, Box::new(List::Ֆ91ՖՖ93Ֆ()))),
    ),
    List::_Ֆ8759Ֆ_(1, Box::new(List::_Ֆ8759Ֆ_(1, Box::new(List::Ֆ91ՖՖ93Ֆ())))),
  ))
}

pub fn constNat(x: i32, x0: i32) -> i32 {
  x
}

pub fn testConstNat() -> i32 {
  sum(map::<i32, i32>(
    |x| constNat(21, x),
    List::_Ֆ8759Ֆ_(1, Box::new(List::_Ֆ8759Ֆ_(2, Box::new(List::Ֆ91ՖՖ93Ֆ())))),
  ))
}

pub fn sumWith(x: impl Fn(i32) -> i32, x0: List<i32>) -> i32 {
  sum(map::<i32, i32>(x, x0))
}

pub fn testSumWith() -> i32 {
  sumWith(
    |x| 1 + x,
    List::_Ֆ8759Ֆ_(40, Box::new(List::_Ֆ8759Ֆ_(0, Box::new(List::Ֆ91ՖՖ93Ֆ())))),
  )
}

pub fn sumWithConst(x: impl Fn(i32, i32) -> i32, x0: List<i32>) -> i32 {
  sum(map::<i32, i32>(|x1| x(21, x1), x0))
}

pub fn testSumWithConst() -> i32 {
  sumWithConst(
    |x, x0| constNat(x, x0),
    List::_Ֆ8759Ֆ_(1, Box::new(List::_Ֆ8759Ֆ_(2, Box::new(List::Ֆ91ՖՖ93Ֆ())))),
  )
}

pub struct SomeNat {
  pub someNat: i32,
}

pub fn SomeNat·someNat(r: SomeNat) -> i32 {
  match r {
    SomeNat { someNat } => someNat,
  }
}

pub fn addSomeNat(x: SomeNat, x0: i32) -> i32 {
  match x {
    SomeNat { someNat } => someNat + x0,
  }
}

pub fn testSomeNat() -> i32 {
  addSomeNat(SomeNat { someNat: 40 }, 2)
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {} | {} | {} | {} | {} | {}", module_path!(),
    constNat(42, 41),
    testPartialAdd(),
    testIncr(),
    testIncr2(),
    testPartialAdd2(),
    testConstNat(),
    testSumWith(),
    testSumWithConst(),
    testSomeNat(),
  );
}
