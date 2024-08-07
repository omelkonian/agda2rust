#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub struct Foo {
  pub foo: i32,
}

pub fn Foo·foo(r: Foo) -> i32 {
  match r {
    Foo { foo } => foo,
  }
}

pub fn getFoo(x: Foo) -> i32 {
  match x {
    Foo { foo } => foo,
  }
}

pub fn _Ֆ43ՖFoo_(x: Foo, x0: Foo) -> Foo {
  Foo { foo: apply!(Foo·foo, x) + apply!(Foo·foo, x0) }
}

pub fn testFoo() -> i32 {
  42
}

pub enum Bar {
  bar(i32),
}

pub fn getBar(x: Bar) -> i32 {
  match x {
    Bar::bar(x0) => x0,
  }
}

pub fn _Ֆ43ՖBar_(x: Bar, x0: Bar) -> Bar {
  match x {
    Bar::bar(x1) => match x0 {
      Bar::bar(x3) => apply!(Bar::bar, x1+x3),
    },
  }
}

pub fn testBar() -> i32 {
  42
}

pub fn main() {
  println!("{}:\t {} | {} | {} | {}", module_path!(),
    testFoo(),
    testBar(),
    getFoo(Foo{foo: 42}),
    getBar(Bar::bar(42)),
  );
}
