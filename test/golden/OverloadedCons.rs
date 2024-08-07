#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,tuple_trait,unboxed_closures,fn_traits,const_trait_impl,effects)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

use unicurry::*;

pub type Email = String;

pub type Password = String;

pub type AccountNo = i32;

pub enum User {
  mk(Email, Password, AccountNo),
}

pub fn accountNo(x: User) -> i32 {
  match x {
    User::mk(x0, x1, x2) => x2,
  }
}

pub fn exUser() -> User {
  apply!(User::mk, "xxx@xxx.com".to_string(), "qwerty".to_string(), 42)
}

pub type Road = String;

pub type RoadNo = i32;

pub type Town = String;

pub type Country = String;

pub enum Address {
  mk(Road, RoadNo, Town, Country),
}

pub fn roadNo(x: Address) -> i32 {
  match x {
    Address::mk(x0, x1, x2, x3) => x1,
  }
}

pub fn exAddress() -> Address {
  apply!(
      Address::mk, "Bay Loan".to_string(), 42, "Morpeth".to_string(
        
      ), "UK".to_string()
  )
}

pub fn main() {
  println!("{}:\t {} | {}", module_path!(),
    accountNo(exUser()),
    roadNo(exAddress()),
  );
}
