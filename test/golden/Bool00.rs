#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub enum Bool {
  r#false(),
  r#true(),
}

pub fn _Ֆ8743Ֆ_(x: Bool, x0: Bool) -> Bool {
  {
    let x1 = Bool::r#false();
    match x {
      Bool::r#true() => match x0 {
        Bool::r#true() => Bool::r#true(),
        _ => x1,
      },
      _ => x1,
    }
  }
}

pub fn rՖ35Ֆtrue() -> Bool {
  Bool::r#true()
}

pub fn rՖ35Ֆfalse() -> Bool {
  Bool::r#false()
}

pub fn testBool() -> Bool {
  apply!(_Ֆ8743Ֆ_, rՖ35Ֆtrue(), rՖ35Ֆfalse())
}

pub fn bool2Nat(x: Bool) -> i32 {
  match x {
    Bool::r#false() => 42,
    Bool::r#true() => 0,
  }
}

pub fn isZero(x: i32) -> Bool {
  match x {
    0 => Bool::r#true(),
    _ => Bool::r#false(),
  }
}

pub fn toNonZero(x: i32) -> i32 {
  {
    let x0 = apply!(isZero, x);
    match x0 {
      Bool::r#false() => x,
      Bool::r#true() => 1,
    }
  }
}

pub fn if_then_else_<A>(x: Bool, x0: A, x1: A) -> A {
  match x {
    Bool::r#false() => x1,
    Bool::r#true() => x0,
  }
}

pub fn testIte() -> i32 {
  apply!(if_then_else_::<i32>, Bool::r#true(), 42, 0)
}

pub fn main () {
  println!("{}:\t\t {} | {} | {} | {}", module_path!(),
    bool2Nat(testBool()),
    bool2Nat(isZero(5)),
    toNonZero(42),
    testIte(),
  );
}
