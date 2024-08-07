#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

#[path = "Agda/Builtin/Maybe.rs"] mod MaybeMod;
use self::MaybeMod::Maybe;
pub fn map<A, B>(x: Rc<dyn Fn(A) -> B>, x0: Maybe<A>) -> Maybe<B> {
  match x0 {
    Maybe::just(x1) => apply!(Maybe::just, apply!(x, x1)),
    Maybe::nothing() => Maybe::nothing(),
  }
}

pub fn zipWith<A, B, C>(
  x: Rc<dyn Fn(A) -> Rc<dyn Fn(B) -> C>>,
  x0: Maybe<A>,
  x1: Maybe<B>,
) -> Maybe<C> {
  {
    let x2 = Maybe::nothing();
    match x0 {
      Maybe::just(x3) => match x1 {
        Maybe::just(x4) => apply!(Maybe::just, apply!(x, x3, x4)),
        Maybe::nothing() => Maybe::nothing(),
      },
      Maybe::nothing() => Maybe::nothing(),
      _ => x2,
    }
  }
}

pub fn incr(x: Maybe<i32>) -> Maybe<i32> {
  apply!(map::<i32, i32>, ᐁF(move|x0|1+x0), x)
}

pub fn incr2(x: Maybe<i32>) -> Maybe<i32> {
  apply!(map::<i32, i32>, ᐁF(move|x0|1+x0), x)
}

pub fn sum(x: Maybe<i32>) -> i32 {
  match x {
    Maybe::just(x0) => x0,
    Maybe::nothing() => 0,
  }
}

pub fn testPartialAdd() -> i32 {
  apply!(sum, apply!(map::<i32, i32>, ᐁF(move|x|1+x), apply!(Maybe::just, 41)))
}

pub fn testIncr() -> i32 {
  apply!(sum, apply!(incr, apply!(Maybe::just, 41)))
}

pub fn testIncr2() -> i32 {
  apply!(sum, apply!(incr2, apply!(Maybe::just, 41)))
}

pub fn testPartialAdd2() -> i32 {
  apply!(
      sum, apply!(
        zipWith::<i32, i32, i32>, ᐁF(move|x|ᐁF(move|x0|x+x0)), apply!(
          Maybe::just, 40
        ), apply!(Maybe::just, 2)
      )
  )
}

pub fn constNat(x: i32, x0: i32) -> i32 {
  x
}

pub fn testConstNat() -> i32 {
  apply!(
      sum, apply!(
        map::<i32, i32>, ᐁF(move|x|apply!(constNat, 42, x)), apply!(
          Maybe::just, 1
        )
      )
  )
}

pub fn sumWith(x: Rc<dyn Fn(i32) -> i32>, x0: Maybe<i32>) -> i32 {
  apply!(sum, apply!(map::<i32, i32>, x, x0))
}

pub fn testSumWith() -> i32 {
  apply!(sumWith, ᐁF(move|x|1+x), apply!(Maybe::just, 40))
}

pub fn sumWithConst(
  x: Rc<dyn Fn(i32) -> Rc<dyn Fn(i32) -> i32>>,
  x0: Maybe<i32>,
) -> i32 {
  apply!(sum, apply!(map::<i32, i32>, ᐁF(move|x1|apply!(x, 42, x1)), x0))
}

pub fn testSumWithConst() -> i32 {
  apply!(
      sumWithConst, ᐁF(move|x|ᐁF(move|x0|apply!(constNat, x, x0))), apply!(
        Maybe::just, 1
      )
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
  apply!(addSomeNat, SomeNat { someNat: 40 } , 2)
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
