#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

mod Identity;
mod Numbers;
mod Simplification;
mod Product;
mod Either;
mod Maybe;
mod Exp;
mod Nat0;
mod List0;
mod Singleton;
mod RunTimeIrrelevance;
mod Postulates;
mod Constants;

fn main() {
  Identity::main();
  Numbers::main();
  Simplification::main();
  Product::main();
  Either::main();
  Maybe::main();
  Exp::main();
  Nat0::main();
  List0::main();
  Singleton::main();
  RunTimeIrrelevance::main();
  Postulates::main();
  Constants::main();
}
