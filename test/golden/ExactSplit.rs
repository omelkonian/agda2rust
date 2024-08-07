#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

#[derive(Debug)]
pub enum Nat {
  zero(),
  suc(Box<Nat>),
}

pub fn min(x: Nat, x0: Nat) -> Nat {
  {
    let x1 = Nat::zero();
    match x {
      Nat::zero() => Nat::zero(),
      Nat::suc(x2) => {
        let x2 = *x2;
        match x0 {
          Nat::zero() => Nat::zero(),
          Nat::suc(x3) => { let x3 = *x3; apply!(min, x2, x3) },
        }
      },
      _ => x1,
    }
  }
}

pub fn main() {
  println!("{}:\t\t {:?}", module_path!(),
    min(Nat::zero(), Nat::zero()),
  );
}
