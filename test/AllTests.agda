import Identity
import Numbers
import Singleton
import Maybe
import Either
import List0
import Nat0
import Product
import Exp

{-# FOREIGN AGDA2RUST
mod Identity;
mod Numbers;
mod Singleton; use Singleton::The::{the};
mod Maybe; use Maybe::Maybe::{Just};
mod Either; use Either::Either::{Left};
mod List0; use List0::List::{Ֆ91ՖՖ93Ֆ,_Ֆ8759Ֆ_};
mod Nat0; use Nat0::Nat::{zero,suc};
mod Product;
mod Exp; use Exp::Exp::{Plus,Int,Var};

fn ᐁ<T>(x : T) -> Box<T> {
  return Box::new(x);
}

fn main() {
  println!("{} | {} | {} | {:?} | {} | {} | | {:?} | {:?} | {:?} | {} | {}",
    Identity::id(42),
    Identity::idՖ10216Ֆ_Ֆ10217Ֆ_(42),
    Numbers::add(40,2),
    the(42),
    Maybe::fromMaybe(41, Just(42)),
    Either::fromEither::<i32, i32>(41, Left(42)),
    List0::map(|x| x + 1, List0::_Ֆ43ՖՖ43Ֆ_(
      _Ֆ8759Ֆ_(3, ᐁ(Ֆ91ՖՖ93Ֆ())),
      _Ֆ8759Ֆ_(1, ᐁ(Ֆ91ՖՖ93Ֆ()))
    )),
    suc(ᐁ(suc(ᐁ(suc(ᐁ(zero())))))),
    Nat0::_Ֆ43Ֆ_(suc(ᐁ(zero())), suc(ᐁ(zero()))),
    Product::mapSnd(
      |x| x + 1
      , Product::_Ֆ215Ֆ_ {projՖ8321Ֆ: 0, projՖ8322Ֆ: 41}
    ).projՖ8322Ֆ,
    Exp::eval(|x| x, Plus(ᐁ(Int(5)),ᐁ(Var(37))))
  );
}
#-}
