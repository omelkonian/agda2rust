#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn id<A>(x: A) -> A {
  x
}

pub fn id0<A>(x: A) -> A {
  x
}

pub fn idՖ10216Ֆ_Ֆ10217Ֆ_<A>(x: A) -> A {
  x
}

pub fn id0Ֆ10216Ֆ_Ֆ10217Ֆ_<A>(x: A) -> A {
  x
}

pub fn idH<A>(x: A) -> A {
  x
}

pub fn id0H<A>(x: A) -> A {
  x
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {} | {} | {}", module_path!(),
    id(42),
    id0(42),
    idՖ10216Ֆ_Ֆ10217Ֆ_(42),
    id0Ֆ10216Ֆ_Ֆ10217Ֆ_(42),
    idH(42),
    id0H(42),
  );
}
