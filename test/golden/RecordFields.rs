#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub enum Wrap<A> {
  mk(A),
}

pub fn unmk<A>(x: Wrap<A>) -> A {
  match x {
    Wrap::mk(x0) => x0,
  }
}

pub struct Point {
  pub slot: Wrap<i32>,
  pub blockHash: i32,
}

pub fn Point·slot(r: Point) -> Wrap<i32> {
  match r {
    Point { slot, blockHash } => slot,
  }
}

pub fn Point·blockHash(r: Point) -> i32 {
  match r {
    Point { slot, blockHash } => blockHash,
  }
}

pub fn matchPoint(x: Point) -> i32 {
  match x {
    Point { slot, blockHash } => match slot {
      Wrap::mk(x0) => x0,
    },
  }
}

pub fn exPoint() -> Point {
  Point { slot: Wrap::mk(42), blockHash: 0 }
}

pub type Hash = i32;

#[derive(Default)]
pub struct Header {
  pub slotNo: i32,
  pub blockNo: i32,
  pub blockHash: Hash,
  pub prev: i32,
  pub nodeId: i32,
}

pub fn Header·slotNo(r: Header) -> i32 {
  match r {
    Header { slotNo, blockNo, blockHash, prev, nodeId } => slotNo,
  }
}

pub fn Header·blockNo(r: Header) -> i32 {
  match r {
    Header { slotNo, blockNo, blockHash, prev, nodeId } => blockNo,
  }
}

pub fn Header·blockHash(r: Header) -> Hash {
  match r {
    Header { slotNo, blockNo, blockHash, prev, nodeId } => blockHash,
  }
}

pub fn Header·prev(r: Header) -> i32 {
  match r {
    Header { slotNo, blockNo, blockHash, prev, nodeId } => prev,
  }
}

pub fn Header·nodeId(r: Header) -> i32 {
  match r {
    Header { slotNo, blockNo, blockHash, prev, nodeId } => nodeId,
  }
}

pub fn matchHeader(x: Header) -> i32 {
  match x {
    Header { slotNo, blockNo, blockHash, prev, nodeId } => slotNo,
  }
}

pub fn exHeader() -> Header {
  Header { slotNo: 42, blockNo: 0, blockHash: 0, prev: 0, nodeId: 0 }
}

pub enum Tx {
  inc(),
  dec(),
}

pub fn pred(x: i32) -> i32 {
  match x {
    0 => 0,
    _ => x - 1,
  }
}

pub fn main() {
  println!("{}:\t\t {} | {} | {} | {}", module_path!(),
    unmk(Point·slot(exPoint())),
    matchPoint(exPoint()),
    Header·slotNo(Header {slotNo: 42, .. Default::default()}),
    matchHeader(exHeader()),
  );
}
