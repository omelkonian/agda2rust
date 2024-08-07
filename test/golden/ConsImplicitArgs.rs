#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub enum Bar {
  bar(),
}

pub enum Foo {
  foo(Bar),
}

pub fn fooToNat(x: Foo) -> i32 {
  match x {
    Foo::foo(x0) => 42,
  }
}

pub fn testFoo() -> Foo {
  apply!(Foo::foo, Bar::bar())
}

pub enum BarA<A> {
  bar(A),
}

pub enum FooA<A> {
  foo(BarA<A>),
}

pub fn fooAToNat(x: FooA<i32>) -> i32 {
  match x {
    FooA::foo(x0) => match x0 {
      BarA::bar(x1) => x1,
    },
  }
}

pub fn testFooA() -> FooA<i32> {
  apply!(FooA::foo, apply!(BarA::bar, 42))
}

pub struct BarR { }

pub struct FooR {
  pub getBar: BarR,
}

pub fn FooR·getBar(r: FooR) -> BarR {
  match r {
    FooR { getBar } => getBar,
  }
}

pub fn fooRToNat(x: FooR) -> i32 {
  match x {
    FooR { getBar } => 42,
  }
}

pub fn testFooR() -> FooR {
  FooR { getBar: BarR { } }
}

pub struct BarRA<A> {
  pub _phantom: std::marker::PhantomData<(A,)>,
}

pub struct FooRA<A> {
  pub getBar: BarRA<A>,
}

pub fn FooRA·getBar<A>(r: FooRA<A>) -> BarRA<A> {
  match r {
    FooRA { getBar } => getBar,
  }
}

pub fn fooRAToNat(x: FooRA<i32>) -> i32 {
  match x {
    FooRA { getBar } => 42,
  }
}

pub fn testFooRA() -> FooRA<i32> {
  FooRA { getBar: BarRA { _phantom: std::marker::PhantomData } }
}

pub fn main() {
  println!("{}:\t {} | {} | {} | {}", module_path!(),
    fooToNat(testFoo()),
    fooAToNat(testFooA()),
    fooRToNat(testFooR()),
    fooRAToNat(testFooRA()),
  );
}
