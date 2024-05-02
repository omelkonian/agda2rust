#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub enum Bool {
  r#false(),
  r#true(),
}

pub fn _Ֆ8743Ֆ_(x0: Bool, x1: Bool) -> Bool {
  {
    let x2 = Bool::r#false();
    match x0 {
      Bool::r#true() => match x1 {
        Bool::r#true() => Bool::r#true(),
        _ => x2,
      },
      _ => x2,
    }
  }
}

pub fn rՖ35Ֆtrue() -> Bool {
  Bool::r#true()
}

pub fn rՖ35Ֆfalse() -> Bool {
  Bool::r#false()
}

pub fn testBool() -> Bool {
  _Ֆ8743Ֆ_(rՖ35Ֆtrue(), rՖ35Ֆfalse())
}

pub fn bool2Nat(x0: Bool) -> i32 {
  match x0 {
    Bool::r#false() => 42,
    Bool::r#true() => 0,
  }
}

pub fn main () {
  println!("{}:\t {:?}", module_path!(),
    bool2Nat(testBool())
  );
}
