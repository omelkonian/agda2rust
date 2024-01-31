#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
fn _impossible<A>() -> A { panic!("IMPOSSIBLE") }
mod Identity;
mod Numbers;
mod Maybe; use Maybe::Maybe::{Just};
mod Either; use Either::Either::{Left};
mod List0; use List0::List::{Cons,Nil};
// mod Exp;
// mod List;
// mod Product;
// mod Lambdas;

fn main() {
  println!("{} | {} | {} | {} | {:?}",
    Identity::id("Hi!"),
    Numbers::add(40,2),
    Maybe::fromMaybe(41, Just(42)),
    Either::fromEither::<i32, i32>(41, Left(42)),
    List0::map(|x| x + 1, List0::con(
      Cons(3, Box::new(Nil())),
      Cons(1, Box::new(Nil()))
    )),
  );
}
