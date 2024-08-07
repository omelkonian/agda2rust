#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub fn erasedFunArg(n: i32) -> i32 {
  1 + n
}

pub fn erasedHigherOrderFunArg(x0: i32) -> i32 {
  1 + x0
}

pub fn erasedRec() -> i32 {
  42
}

pub struct ErasedField {
  pub x: i32,
}

pub fn ErasedField·x(r: ErasedField) -> i32 {
  match r {
    ErasedField { x } => x,
  }
}

pub fn succ(x: ErasedField) -> i32 {
  match x {
    ErasedField { x: x0 } => 1 + x0,
  }
}

pub struct ErasedRecParam {
  pub y: i32,
}

pub fn ErasedRecParam·y(r: ErasedRecParam) -> i32 {
  match r {
    ErasedRecParam { y } => y,
  }
}

pub fn erasedRecParam(x0: ErasedRecParam) -> i32 {
  match x0 {
    ErasedRecParam { y } => y,
  }
}

pub fn erasedRecParamH(x0: ErasedRecParam) -> i32 {
  match x0 {
    ErasedRecParam { y } => y,
  }
}

pub fn erasedData() -> i32 {
  42
}

#[derive(Debug)]
pub enum ErasedCon {
  mk(i32),
}

pub fn erasedClause(x: ErasedCon) -> i32 {
  match x {
    ErasedCon::mk(x0) => x0,
  }
}

#[derive(Debug)]
pub enum ErasedConArg {
  mk(i32),
}

pub fn erasedConArg(x: ErasedConArg) -> i32 {
  match x {
    ErasedConArg::mk(n) => n,
  }
}

#[derive(Debug)]
pub enum BST {
  Leaf(),
  Node(i32, Box<BST>, Box<BST>),
}

use self::BST::{Leaf,Node};

pub fn main() {
  println!("{}:\t \
    {} | {} | \
    {} | {} | {} | {} | \
    {} | {:?} | {:?} | {:?}", module_path!(),
    erasedFunArg(41),
    erasedHigherOrderFunArg(41),

    erasedRec(),
    succ(ErasedField {x: 41}),
    erasedRecParam(ErasedRecParam {y: 42}),
    erasedRecParamH(ErasedRecParam {y: 42}),

    erasedData(),
    erasedClause(ErasedCon::mk(42)),
    erasedConArg(ErasedConArg::mk(42)),
    Node(4, ᐁ(Node(2, ᐁ(Leaf()), ᐁ(Leaf()))), ᐁ(Leaf())),
  );
}
