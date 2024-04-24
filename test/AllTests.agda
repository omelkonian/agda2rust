import Identity
import Numbers
import Simplification
import Product
import Either
import Maybe
import Exp
import Nat0
import List0
import Singleton

-- ** FUTURE
-- import NonLinearFun
-- import Eq
-- import Eq0
-- import Vec
-- import Erasure
-- import RunTimeIrrelevance
-- import CompileTimeIrrelevance
-- import FFI

{-# FOREIGN AGDA2RUST
mod Identity;
mod Numbers;
mod Simplification;
mod Product;
mod Either;
mod Maybe; use Maybe::Maybe::{Just};
mod Exp; use Exp::Exp::{Plus,Int,Var};
mod Nat0; use Nat0::Nat::{zero,suc};
mod List0; use List0::List::{Ֆ91ՖՖ93Ֆ,_Ֆ8759Ֆ_};
mod Singleton; use Singleton::The::{the};
// mod NonLinearFun; use NonLinearFun::double;

fn ᐁ<T>(x : T) -> Box<T> {
  return Box::new(x);
}

use std::marker::{PhantomData};
fn __<T>() -> PhantomData<T> {
  return PhantomData;
}

fn main() {
  println!(
    "{} | {} | {} | {} | {} | | {} | {} | {} | {} | {} | {:?} | {:?} | {:?} | {:?}",
    // *** Identity ***
    Identity::id(42),
    Identity::idՖ10216Ֆ_Ֆ10217Ֆ_(42),
    // Base types
    Numbers::add(40,2),
    Simplification::increment(41),
    // *** Records ***
    Product::mapSnd(
      |x| x + 1
      , Product::_Ֆ215Ֆ_ {projՖ8321Ֆ: 0, projՖ8322Ֆ: 41}
    ).projՖ8322Ֆ,
    // *** ADTs ***
    Either::fromEither::<i32, i32>(41, Either::Either::Left(42)),
    Either::fromOnlyLeft::<i32, i32>(Either::OnlyLeft::Left(42)),
    Either::fromOnlyLeftR::<i32, i32>(Either::OnlyLeftR {left: 42, _phantom: __()}),
    Maybe::fromMaybe(41, Just(42)),
    // *** Resursive ADTs ***
    Exp::eval(|x| x, Plus(ᐁ(Int(5)),ᐁ(Var(37)))),
    suc(ᐁ(suc(ᐁ(suc(ᐁ(suc(ᐁ(zero())))))))),
    Nat0::_Ֆ43Ֆ_(suc(ᐁ(zero())), suc(ᐁ(zero()))),
    List0::map(|x| x + 1, List0::_Ֆ43ՖՖ43Ֆ_(
      _Ֆ8759Ֆ_(3, ᐁ(Ֆ91ՖՖ93Ֆ())),
      _Ֆ8759Ֆ_(1, ᐁ(Ֆ91ՖՖ93Ֆ()))
    )),
    // *** Value-dependent types ***
    the(42),
    // *** Non-linear functions ***
    // double(21)
  );
}
#-}
