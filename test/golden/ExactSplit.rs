#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

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
          Nat::suc(x3) => { let x3 = *x3; min(x2, x3) },
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
