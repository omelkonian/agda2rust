#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

#[path = "Agda/Builtin/List.rs"] mod ListMod;
use self::ListMod::List;
pub fn sum(x: List<i32>) -> i32 {
  match x {
    List::Ֆ91ՖՖ93Ֆ() => 0,
    List::_Ֆ8759Ֆ_(x0, xs) => { let xs = *xs; apply!(sum, xs) + x0 },
  }
}

pub fn testSum() -> i32 {
  apply!(
      sum, apply!(
        List::_Ֆ8759Ֆ_, 30, ᐁ(apply!(List::_Ֆ8759Ֆ_, 12, ᐁ(List::Ֆ91ՖՖ93Ֆ())))
      )
  )
}

pub fn _Ֆ43ՖՖ43Ֆ_<A>(x: List<A>, x0: List<A>) -> List<A> {
  match x {
    List::Ֆ91ՖՖ93Ֆ() => x0,
    List::_Ֆ8759Ֆ_(x1, xs) => {
      let xs = *xs;
      apply!(List::_Ֆ8759Ֆ_, x1, ᐁ(apply!(_Ֆ43ՖՖ43Ֆ_::<A>, xs, x0)))
    },
  }
}

pub fn map<A, B>(x: Rc<dyn Fn(A) -> B>, x0: List<A>) -> List<B> {
  match x0 {
    List::Ֆ91ՖՖ93Ֆ() => List::Ֆ91ՖՖ93Ֆ(),
    List::_Ֆ8759Ֆ_(x1, xs) => {
      let xs = *xs;
      apply!(List::_Ֆ8759Ֆ_, apply!(x, x1), ᐁ(apply!(map::<A, B>, x, xs)))
    },
  }
}

pub fn zipWith<A, B, C>(
  x: Rc<dyn Fn(A) -> Rc<dyn Fn(B) -> C>>,
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
            apply!(
                List::_Ֆ8759Ֆ_, apply!(x, x3, x4), ᐁ(
                  apply!(zipWith::<A, B, C>, x, xs, xs0)
                )
            )
          },
        }
      },
      _ => x2,
    }
  }
}

use self::List::{Ֆ91ՖՖ93Ֆ,_Ֆ8759Ֆ_};

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {}", module_path!(),
    testSum(),
    sum(_Ֆ43ՖՖ43Ֆ_(
     _Ֆ8759Ֆ_(40, ᐁ(Ֆ91ՖՖ93Ֆ())),
     _Ֆ8759Ֆ_(2, ᐁ(Ֆ91ՖՖ93Ֆ()))
    )),
    sum(map(ᐁF(move |x| x + 1), _Ֆ43ՖՖ43Ֆ_(
     _Ֆ8759Ֆ_(39, ᐁ(Ֆ91ՖՖ93Ֆ())),
     _Ֆ8759Ֆ_(1, ᐁ(Ֆ91ՖՖ93Ֆ()))
    ))),
    sum(zipWith(ᐁF(move |x| ᐁF(move |y| x + y)),
     _Ֆ8759Ֆ_(40, ᐁ(Ֆ91ՖՖ93Ֆ())),
     _Ֆ8759Ֆ_(2, ᐁ(Ֆ91ՖՖ93Ֆ()))
    )),
  );
}
