#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
pub enum Exp<  V,> {
  Plus(Box<Exp<V>>, Box<Exp<V>>),
  Int(i32),
  Var(V),
  _Impossible(std::marker::PhantomData<(V,)>),
}



pub fn eval<  A,>(x0: fn(_: A) -> i32, x1: Exp<A>) -> i32 {
  match x1 {
    Exp::Plus(x2, x3) => {
      let x2 = *x2;
      let x3 = *x3;
      eval(x0, x2) + eval(x0, x3)
    },
    Exp::Int(x2) => x2,
    Exp::Var(x2) => x0(x2),
    _ => panic!("IMPOSSIBLE"),
  }
}
