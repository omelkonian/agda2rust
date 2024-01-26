#![allow(dead_code, non_snake_case, unused_variables)]
fn _impossible<A>() -> A { panic!("IMPOSSIBLE") }
mod Identity;
mod Numbers;
mod Maybe;
mod Either;
// mod Exp;
// mod List;
// mod Product;
// mod Lambdas;

fn main() {
  println!("{} {}", Identity::id("Hi!"), Numbers::add(40,2));
}
