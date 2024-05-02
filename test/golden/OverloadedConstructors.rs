#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub type Email = String;

pub type Password = String;

pub type AccountNo = i32;

pub enum User {
  mk(Email, Password, AccountNo),
}

pub fn accountNo(x0: User) -> i32 {
  match x0 {
    User::mk(x1, x2, x3) => x3,
  }
}

pub fn exUser() -> User {
  User::mk("xxx@xxx.com".to_string(), "qwerty".to_string(), 42)
}

pub type Road = String;

pub type RoadNo = i32;

pub type Town = String;

pub type Country = String;

pub enum Address {
  mk(Road, RoadNo, Town, Country),
}

pub fn roadNo(x0: Address) -> i32 {
  match x0 {
    Address::mk(x1, x2, x3, x4) => x2,
  }
}

pub fn exAddress() -> Address {
  Address::mk(
    "Bay Loan".to_string(),
    42,
    "Morpeth".to_string(),
    "UK".to_string(),
  )
}

pub fn main() {
  println!("{}: {} | {}", module_path!(),
    accountNo(exUser()),
    roadNo(exAddress()),
  );
}
