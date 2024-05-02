#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

mod Identity;
mod Numbers;
mod Simplification;
mod Product;
mod Either;
mod Maybe;
mod Exp;
mod Nat0;
mod Nat;
mod List0;
mod Singleton;
mod RunTimeIrrelevance;
mod Postulates;
mod Constants;
// mod Float0;
mod Float;
mod Char;
// mod String0;
mod String;
mod Prim;
mod Bool00;
// mod Bool0;
mod Bool;
mod TypeAliases;
mod OverloadedConstructors;
mod OverloadedFields;

fn main() {
  Identity::main();
  Numbers::main();
  Simplification::main();
  Product::main();
  Either::main();
  Maybe::main();
  Exp::main();
  Nat0::main();
  Nat::main();
  List0::main();
  Singleton::main();
  RunTimeIrrelevance::main();
  Postulates::main();
  Constants::main();
  // Float0::main();
  Float::main();
  Char::main();
  // String0::main();
  String::main();
  Prim::main();
  Bool00::main();
  // Bool0::main();
  Bool::main();
  TypeAliases::main();
  OverloadedConstructors::main();
  OverloadedFields::main();
}
