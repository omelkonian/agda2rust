#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub fn id<A>(x: A) -> A {
  x
}

pub fn it<A>(x: A) -> A {
  id::<A>(x)
}

pub fn k<A, B>(x: A, x0: B) -> A {
  x
}

pub fn drop<A, B>(x: A, x0: B) -> A {
  k::<A, B>(x, x0)
}

pub fn main() {
  println!("{}:\t\t\t {} | {} | {} | {}", module_path!(),
    id(42),
    it(42),
    k(42, 0),
    drop(42, 0)
  );
}
