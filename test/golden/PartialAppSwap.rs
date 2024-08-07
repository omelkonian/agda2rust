#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn swapF<A, B>(x: Rc<dyn Fn(A) -> Rc<dyn Fn(A) -> B>>, x0: A, x1: A) -> B {
  apply!(x, x1, x0)
}

pub fn testSwapAdd() -> i32 {
  apply!(swapF::<i32, i32>, ᐁF(move|x|ᐁF(move|x0|x+x0)), 40, 2)
}

pub fn testSwapAdd2(x: i32) -> i32 {
  apply!(swapF::<i32, i32>, ᐁF(move|x0|ᐁF(move|x1|x0+x1)), x, 2)
}

pub fn testSwapSwap() -> i32 {
  apply!(
      swapF::<i32, i32>, ᐁF(
        move|x|ᐁF(
          move|x0|apply!(
            swapF::<i32, i32>, ᐁF(move|x1|ᐁF(move|x2|x1+x2)), x, x0
          )
        )
      ), 40, 2
  )
}

pub fn add3(x: i32, x0: i32, x1: i32) -> i32 {
  x + x0 + x1
}

pub fn testSwapAdd3() -> i32 {
  apply!(
      swapF::<i32, _>, ᐁF(
        move|x|ᐁF(move|x0|ᐁF(move|x1|apply!(add3, x, x0, x1)))
      ), 40, 1, 1
  )
}

pub fn testSwapSwap3() -> i32 {
  apply!(
      swapF::<i32, _>, ᐁF(
        move|x|ᐁF(
          move|x0|apply!(
            swapF::<i32, _>, ᐁF(
              move|x1|ᐁF(move|x2|ᐁF(move|x3|apply!(add3, x1, x2, x3)))
            ), x, x0
          )
        )
      ), 40, 1, 1
  )
}

pub fn main() {
  println!("{}:\t {} | {} | {} | {} | {}", module_path!(),
    testSwapAdd(),
    testSwapAdd2(40),
    testSwapSwap(),
    testSwapAdd3(),
    testSwapSwap3(),
  );
}
