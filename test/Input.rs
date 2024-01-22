#![allow(dead_code, non_snake_case)]

// enum Message {
//     Quit,
//     Move { x: i32, y: i32 },
//     Write(String),
//     ChangeColor(i32, i32, i32),
// }

// enum TupleVariants {
//     None(),
//     One(i32),
//     Two(i32,i32)
// }

// enum Option<T> {
//     None,
//     Some(T),
// }

// enum List<T> {
//     Nil,
//     Cons(T, List<T>),
// }

// #[derive(std::fmt::Display)]
// #[derive(Debug)]

enum Maybe<A> {
  Nothing(),
  Just(A),
}

// fn m0() -> Maybe<i32> {
//   Maybe::Nothing()
// }

// fn m1() -> Maybe<i32> {
//   Maybe::Just(1)
// }

// fn fromMaybe(m: Maybe<i32>) -> i32 {
//   match m {
//     Maybe::Nothing() => 0
//   , Maybe::Just(x)   => x
//   }
// }

fn maybeToBool(x0: Maybe<i32>) -> i32 {
  { let x = 1; match x0 { Maybe::Nothing() => 0, _ => x } }
}

fn main() {
  let x: i32 = maybeToBool(Maybe::Just(42));
  // println!("test let")
  println!("{}", x)
}

// fn fromMaybe<A>(def: A, m: Maybe<A>) -> A {
//   match m {
//     Maybe::Nothing() => def
//   , Maybe::Just(x)   => x
//   }
// }

// fn main() {
//   // let x: Maybe<i32> = Maybe::Nothing();
//   // let y: Maybe<i32> = Maybe::Just(5);
//   let x = m0();
//   let y = m1();
//   println!("{:#?} | {:#?}", x, y);
// }
