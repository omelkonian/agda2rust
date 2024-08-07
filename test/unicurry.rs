#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait, unboxed_closures, fn_traits)]
#![allow(dead_code,non_snake_case)]

pub use std::marker::{PhantomData, Tuple};
pub use std::rc::Rc;

/* Useful shorthands */
pub type ᐁ<A> = Box<A>;
pub fn ᐁ<T>(x : T) -> ᐁ<T> { return ᐁ::new(x); }

pub type ᐁF<A> = Rc<A>;
pub fn ᐁF<T>(x : T) -> ᐁF<T> { return ᐁF::new(x); }

pub fn __<T>() -> PhantomData<T> { return PhantomData; }

/* Thomas' unicurry */
use tupleops::{concat_tuples, ConcatTuples, TuplePrepend};
pub trait Unicurry<A, B> {
  type Output;
  fn unicurry(self, a: A) -> Self::Output;
}

impl<A, F: FnOnce<(A,)>> Unicurry<A, ()> for F {
  type Output = F::Output;
  fn unicurry(self, a: A) -> Self::Output {
    self(a)
  }
}

macro_rules! unicurry_many {
  ($t1:ident, $t2:ident$(, $ty:ident)*) => {
    impl<$t1, $t2, $($ty,)* FN: FnOnce<($t1, $t2, $($ty,)*)>> Unicurry<$t1, ($t2, $($ty,)*)> for FN {
      type Output = Curried<$t1, ($t2, $($ty,)*), FN>;
      fn unicurry(self, arg: $t1) -> Self::Output {
        Curried { arg, f: self, _phantom: PhantomData }
      }
    }
    unicurry_many!($t2$(, $ty)*);
  };
  ($t1:ident) => {};
}

unicurry_many!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);

pub struct Curried<A, B: Tuple, F: FnOnce<ConcatTuples<(A,), B>>>
where
  (A, B): TuplePrepend<A, B>,
  ConcatTuples<(A,), B>: Tuple,
{
  arg: A,
  f: F,
  _phantom: PhantomData<B>,
}

impl<A, B: Tuple, F: FnOnce<ConcatTuples<(A,), B>>> FnOnce<B> for Curried<A, B, F>
where
  (A, B): TuplePrepend<A, B>,
  ConcatTuples<(A,), B>: Tuple,
{
  type Output = F::Output;
  extern "rust-call" fn call_once(self, args: B) -> Self::Output {
      self.f.call_once(concat_tuples((self.arg,), args))
  }
}

#[macro_export]
macro_rules! apply {
  ($f:expr$(, $arg:expr)*) => {
      $f$(.unicurry($arg))*
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn curry() {
    fn foo(x: u32, y: u32, z: u32) -> u32 {
        x * y + z
    }
    assert_eq!(apply!(foo, 5, 5, 5), 30);

    fn fooC(x: u32) -> impl Fn(u32) -> Box<dyn Fn(u32) -> u32> {
        move |y| Box::new(move |z| x * y + z)
    }
    assert_eq!(apply!(fooC, 5, 5, 5), 30);

    fn id<T>(x: T) -> T {
        x
    }
    fn map(f: impl Fn(u32) -> u32, opt: Option<u32>) -> Option<u32> {
        opt.map(f)
    }
    fn double(x: u32) -> u32 {
        x * 2
    }
    assert_eq!(
        apply!(id, map, double, Some(5)),
        Some(10)
    );

    // fn mul(x: u32, y: u32) -> u32 {
    //     x * y
    // }
    // assert_eq!(
    //     c!(id, map, c!(mul, 2), Some(5)),
    //     Some(10)
    // );

    // fn swap<T, U>(f: impl Fn(T, T) -> U, x: T, y: T) -> U {
    //     f(y,x)
    // }
    fn swap<T, U>(f: Box<dyn Fn(T) -> Box<dyn Fn(T) -> U>>, x: T, y: T) -> U {
        f(y)(x)
    }
    fn idDyn<A>(f: Box<dyn Fn(A) -> A>, x: A) -> A {
      f(x)
    }
    // assert_eq!(apply!(swap, foo, 5, 5, 5), 30);
    // assert_eq!(apply!(swap, move |x, y, z| foo(x,y,z), 5, 5, 5), 30);
    assert_eq!(apply!(swap, Box::new(move |x| Box::new(move |y| move |z| foo(x,y,z))), 5, 5, 5), 30);
  }
}