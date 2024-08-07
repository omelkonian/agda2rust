#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

mod Identity;
mod Numbers;
mod Simplification;
mod Top;
mod Newtype;
mod Product;
mod Either;
mod Maybe;
mod Nat0;
mod Nat;
mod List0;
mod List;
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
mod Int;
mod TypeAliases;
mod OverloadedCons;
mod OverloadedFields;
mod ErasedFields;
mod RecordFields;
mod UnusedArgs;
mod ExactSplit;
mod OpenModule;
mod ConsImplicitArgs;

mod MultiArgFun;
mod Curry;
mod PointFree;
mod PartialApp;
mod PartialAppM;
mod PartialAppId;
mod EtaExpansion;
mod PartialAppSwap;

// mod Closures;
// mod PartialAppConst;
// mod PartialAppSwapId;

// mod NonLinearity;
// mod Exp;

fn main() {
  Identity::main();
  Numbers::main();
  Simplification::main();
  Top::main();
  Newtype::main();
  Product::main();
  Either::main();
  Maybe::main();
  Nat0::main();
  Nat::main();
  List0::main();
  List::main();
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
  Int::main();
  TypeAliases::main();
  OverloadedCons::main();
  OverloadedFields::main();
  ErasedFields::main();
  RecordFields::main();
  UnusedArgs::main();
  ExactSplit::main();
  OpenModule::main();
  ConsImplicitArgs::main();

  MultiArgFun::main();
  Curry::main();
  PointFree::main();
  PartialApp::main();
  PartialAppM::main();
  PartialAppId::main();
  EtaExpansion::main();
  PartialAppSwap::main();

//  Closures::main();
//  PartialAppConst::main();
//  PartialAppSwapId::main();

//  NonLinearity::main();
//  Exp::main();
}
