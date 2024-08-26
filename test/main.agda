import Identity
import Numbers
import Simplification
import Top
import Newtype
import Product
import Either
import Maybe
import Nat0
import Nat
import List0
import List
import Singleton
import RunTimeIrrelevance
import Postulates
import Constants
-- import Float0
import Float
import Char
-- import String0
import String
import Prim
import Bool00
-- import Bool0
import Bool
import Int
import TypeAliases
import OverloadedCons
import OverloadedFields
import ErasedFields
import RecordFields
import UnusedArgs
import ExactSplit
import OpenModule
import ConsImplicitArgs
import Where
import Levels

-- ** partial application / currying
import MultiArgFun
import Curry
import PointFree
import PartialApp
import PartialAppM
import PartialAppId
import EtaExpansion
import PartialAppSwap

-- ** closures
-- import Closures
-- import PartialAppConst
-- import PartialAppSwapId

-- ** non-linearity
-- import NonLinearity
-- import Exp

{-# FOREIGN AGDA2RUST
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
mod Where;
mod Levels;

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
  Where::main();
  Levels::main();

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
#-}
