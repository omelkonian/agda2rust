#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn id<A>(x: A) -> A {
  x
}

pub fn idd<A>(x: Rc<dyn Fn(A) -> A>, x0: A) -> A {
  apply!(x, x0)
}

pub fn testIdd() -> i32 {
  apply!(idd::<i32>, ᐁF(move|x|apply!(id::<i32>, x)), 42)
}

pub fn testIdId() -> i32 {
  apply!(id::<_>, ᐁF(move|x|apply!(id::<i32>, x)), 42)
}

pub fn testIdIdId() -> i32 {
  apply!(
    id::<_>, ᐁF(move|x|apply!(id::<_>, x)), ᐁF(move|x|apply!(id::<i32>, x)), 42
  )
}

pub fn testNestedId() -> i32 {
  apply!(
      id::<_>, apply!(id::<_>, ᐁF(move|x|apply!(id::<_>, x))), apply!(
        id::<_>, ᐁF(move|x|apply!(id::<_>, x)), ᐁF(move|x|apply!(id::<i32>, x))
      ), apply!(id::<_>, ᐁF(move|x|apply!(id::<i32>, x)), 42)
  )
}

pub fn testIdAdd() -> i32 {
  apply!(id::<_>, ᐁF(move|x|ᐁF(move|x0|x+x0)), 40, 2)
}

pub fn add3(x: i32, x0: i32, x1: i32) -> i32 {
  x + x0 + x1
}

pub fn testIdAdd3() -> i32 {
  apply!(
      id::<_>, ᐁF(
        move|x|ᐁF(move|x0|ᐁF(move|x1|apply!(add3, x, x0, x1)))
      ), 40, 1, 1
  )
}

pub fn idF(x: Rc<dyn Fn(i32) -> Rc<dyn Fn(i32) -> i32>>) -> i32 {
  apply!(id::<_>, x, 40, 2)
}

pub fn testIdF() -> i32 {
  apply!(idF, ᐁF(move|x|ᐁF(move|x0|x+x0)))
}

pub fn idF3(
  x: Rc<dyn Fn(i32) -> Rc<dyn Fn(i32) -> Rc<dyn Fn(i32) -> i32>>>,
) -> i32 {
  apply!(id::<_>, x, 40, 1, 1)
}

pub fn testIdF3() -> i32 {
  apply!(idF3, ᐁF(move|x|ᐁF(move|x0|ᐁF(move|x1|apply!(add3, x, x0, x1)))))
}

pub fn main() {
  println!("{}:\t {} | {} | {} | {} |\
    {} | {} | {} | {}", module_path!(),
    testIdd(),
    testIdId(),
    testIdIdId(),
    testNestedId(),

    testIdAdd(),
    testIdAdd3(),
    testIdF(),
    testIdF3(),
  );
}
