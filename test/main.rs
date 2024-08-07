#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,unsized_locals,tuple_trait, unboxed_closures, fn_traits)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,incomplete_features,uncommon_codepoints, unused_imports)]

use unicurry::*;

// ** Lists
#[derive(Debug, Clone)]
enum List<A> {
  nil(),
  cons(A, ᐁ<List<A>>),
}

fn map<A, B>(f: &dyn Fn(A) -> B, xs: List<A>) -> List<B> {
// fn map<A, B>(f: &impl Fn(A) -> B, xs: List<A>) -> List<B> {
  match xs {
    List::nil() => List::nil(),
    List::cons(x1, xs) => {
      let xs = *xs;
      List::cons(f(x1), ᐁ(map::<A, B>(f, xs)))
    },
  }
}

pub fn id<A>(x: A) -> A {
  x
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

      fn mul(x: u32, y: u32) -> u32 {
          x * y
      }
      // assert_eq!(
      //     apply!(id, map, c!(mul, 2), Some(5)),
      //     Some(10)
      // );
  }
}

fn main() {
    println!("Run tests using `cargo test`");
}