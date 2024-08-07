#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

#[derive(Debug)]
pub enum List<A> {
  nil(),
  cons(A, Box<List<A>>),
}

pub fn cat<A>(x: List<A>, x0: List<A>) -> List<A> {
  match x {
    List::nil() => x0,
    List::cons(x1, xs) => {
      let xs = *xs;
      apply!(List::cons, x1, ᐁ(apply!(cat::<A>, xs, x0)))
    },
  }
}

pub fn map<A, B>(x: Rc<dyn Fn(A) -> B>, x0: List<A>) -> List<B> {
  match x0 {
    List::nil() => List::nil(),
    List::cons(x1, xs) => {
      let xs = *xs;
      apply!(List::cons, apply!(x, x1), ᐁ(apply!(map::<A, B>, x, xs)))
    },
  }
}

pub fn flatten<A>(x: List<List<A>>) -> List<A> {
  match x {
    List::nil() => List::nil(),
    List::cons(x0, xs) => {
      let xs = *xs;
      apply!(cat::<A>, x0, apply!(flatten::<A>, xs))
    },
  }
}

pub fn head<A>(x: List<List<A>>) -> List<A> {
  match x {
    List::nil() => List::nil(),
    List::cons(x0, xs) => {
      let xs = *xs;
      match x0 {
        List::nil() => List::nil(),
        List::cons(x1, xs0) => {
          let xs0 = *xs0;
          apply!(List::cons, x1, ᐁ(List::nil()))
        },
      }
    },
  }
}

pub const empty: List<i32> = List::nil();

pub fn headNum(x: List<i32>) -> i32 {
  match x {
    List::nil() => 42,
    List::cons(x0, xs) => { let xs = *xs; x0 },
  }
}

pub fn single(x: i32) -> List<i32> {
  apply!(List::cons, x, ᐁ(List::nil()))
}

pub fn sum(x: List<i32>) -> i32 {
  match x {
    List::nil() => 0,
    List::cons(x0, xs) => { let xs = *xs; apply!(sum, xs) + x0 },
  }
}

pub fn testFlatten() -> i32 {
  apply!(
      sum, apply!(
        flatten::<i32>, apply!(
          List::cons, apply!(List::cons, 20, ᐁ(List::nil())), ᐁ(
            apply!(
              List::cons, apply!(List::cons, 20, ᐁ(List::nil())), ᐁ(
                apply!(
                  List::cons, apply!(List::cons, 2, ᐁ(List::nil())), ᐁ(
                    List::nil()
                  )
                )
              )
            )
          )
        )
      )
  )
}

pub fn testHead() -> List<i32> {
  apply!(
      head::<i32>, apply!(
        List::cons, apply!(
          List::cons, 42, ᐁ(
            apply!(List::cons, 2, ᐁ(apply!(List::cons, 3, ᐁ(List::nil()))))
          )
        ), ᐁ(
          apply!(
            List::cons, List::nil(), ᐁ(
              apply!(List::cons, List::nil(), ᐁ(List::nil()))
            )
          )
        )
      )
  )
}

use self::List::{nil,cons};

pub fn main() {
  println!("{}:\t\t {:?} | {:?} | {:?} | {} | {:?}", module_path!(),
    headNum(empty),
    single(42),
    map(ᐁF(|x| x + 1), cat(
      cons(3, ᐁ(nil())),
      cons(1, ᐁ(nil()))
    )),
    testFlatten(),
    testHead(),
  );
}
