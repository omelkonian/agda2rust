#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
mod Identity;
mod Numbers;
mod Singleton; use Singleton::The::{the};
mod Maybe; use Maybe::Maybe::{Just};
mod Either; use Either::Either::{Left};
mod List0; use List0::List::{Ֆ91ՖՖ93Ֆ,_Ֆ8759Ֆ_};
mod Product;
mod Exp; use Exp::Exp::{Plus,Int,Var};

fn main() {
  println!("{} | {} | {} | {:?} | {} | {} | {:?} | {} | {}",
    Identity::id("Hi!"),
    Identity::idՖ10216Ֆ_Ֆ10217Ֆ_("Hi!"),
    Numbers::add(40,2),
    the(42),
    Maybe::fromMaybe(41, Just(42)),
    Either::fromEither::<i32, i32>(41, Left(42)),
    List0::map(|x| x + 1, List0::_Ֆ43ՖՖ43Ֆ_(
      _Ֆ8759Ֆ_(3, Box::new(Ֆ91ՖՖ93Ֆ())),
      _Ֆ8759Ֆ_(1, Box::new(Ֆ91ՖՖ93Ֆ()))
    )),
    Product::mapSnd(
      |x| x + 1
      , Product::_Ֆ215Ֆ_ {projՖ8321Ֆ: 0, projՖ8322Ֆ: 41}
    ).projՖ8322Ֆ,
    Exp::eval(|x| x, Plus(Box::new(Int(5)),Box::new(Var(37))))
  );
}
