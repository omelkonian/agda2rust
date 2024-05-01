#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub fn erasedFunArg(n: i32) -> i32 {
  1 + n
}

pub fn erasedHigherOrderFunArg(x1: i32) -> i32 {
  1 + x1
}

pub fn erasedRec() -> i32 {
  42
}

pub struct ErasedField {
  pub x: i32,
}

pub fn x(r: ErasedField) -> i32 {
  match r {
    ErasedField { x } => x,
  }
}

pub fn succ(x0: ErasedField) -> i32 {
  match x0 {
    ErasedField { x } => 1 + x,
  }
}

pub struct ErasedRecParam {
  pub y: i32,
}

pub fn y(r: ErasedRecParam) -> i32 {
  match r {
    ErasedRecParam { y } => y,
  }
}

pub fn erasedRecParam(x0: ErasedRecParam) -> i32 {
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

pub fn erasedClause(x0: ErasedCon) -> i32 {
  match x0 {
    ErasedCon::mk(x1) => x1,
  }
}

#[derive(Debug)]
pub enum ErasedConArg {
  mk(i32),
}

pub fn erasedConArg(x0: ErasedConArg) -> i32 {
  match x0 {
    ErasedConArg::mk(n) => n,
  }
}

#[derive(Debug)]
pub enum BST {
  Leaf(),
  Node(i32, Box<BST>, Box<BST>),
}

fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

use self::BST::{Leaf,Node};

pub fn main() {
  println!("{}: {} | {} | {} | {} | {} | {} | {:?} | {:?} | {:?}", module_path!(),
    erasedFunArg(41),
    erasedHigherOrderFunArg(41),
    erasedRec(),
    succ(ErasedField {x: 41}),
    erasedRecParam(ErasedRecParam {y: 42}),
    erasedData(),
    erasedClause(ErasedCon::mk(42)),
    erasedConArg(ErasedConArg::mk(42)),
    Node(4, ᐁ(Node(2, ᐁ(Leaf()), ᐁ(Leaf()))), ᐁ(Leaf())),
  );
}
