#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub enum Exp<  V,> {
  Plus(Box<Exp<V>>, Box<Exp<V>>),
  Int(i32),
  Var(V),
}

pub fn exampleExp() -> Exp<i32> {
  Exp::Plus(Box::new(Exp::Int(5)), Box::new(Exp::Var(37)))
}

pub fn eval<  A,>(x0: fn(_: A) -> i32, x1: Exp<A>) -> i32 {
  match x1 {
    Exp::Plus(x2, x3) => {
      let x2 = *x2;
      let x3 = *x3;
      eval::<A>(x0, x2) + eval::<A>(x0, x3)
    },
    Exp::Int(x2) => x2,
    Exp::Var(x2) => x0(x2),
  }
}

fn ᐁ<T>(x : T) -> Box<T> { return Box::new(x); }

use self::Exp::{Plus,Int,Var};

pub fn main() {
  println!("{}: {} | {}", module_path!(),
    eval(|x| x, exampleExp()),
    eval(|x| x + 1, Plus(ᐁ(Int(5)),ᐁ(Var(36)))),
  );
}
