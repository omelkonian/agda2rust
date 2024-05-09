#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub struct Foo {
  pub foo: i32,
}

pub fn Foo·foo(r: Foo) -> i32 {
  match r {
    Foo { foo } => foo,
  }
}

pub fn getFoo(x0: Foo) -> i32 {
  match x0 {
    Foo { foo } => foo,
  }
}

pub fn _Ֆ43ՖFoo_(x0: Foo, x1: Foo) -> Foo {
  Foo { foo: Foo·foo(x0) + Foo·foo(x1) }
}

pub fn testFoo() -> i32 {
  42
}

pub enum Bar {
  bar(i32),
}

pub fn getBar(x0: Bar) -> i32 {
  match x0 {
    Bar::bar(x1) => x1,
  }
}

pub fn _Ֆ43ՖBar_(x0: Bar, x1: Bar) -> Bar {
  match x0 {
    Bar::bar(x2) => match x1 {
      Bar::bar(x4) => Bar::bar(x2 + x4),
    },
  }
}

pub fn testBar() -> i32 {
  42
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {}", module_path!(),
    testFoo(),
    testBar(),
    getFoo(Foo{foo: 42}),
    getBar(Bar::bar(42)),
  );
}
