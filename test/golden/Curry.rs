#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn id<A>(x: A) -> A {
  x
}

pub fn it<A>(x: A) -> A {
  apply!(id::<A>, x)
}

pub fn k<A, B>(x: A, x0: B) -> A {
  x
}

pub fn drop<A, B>(x: A, x0: B) -> A {
  apply!(k::<A, B>, x, x0)
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {}", module_path!(),
    id(42),
    it(42),
    k(42, 0),
    drop(42, 0)
  );
}
