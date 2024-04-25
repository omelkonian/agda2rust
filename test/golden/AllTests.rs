#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]
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
mod RunTimeIrrelevance;
use RunTimeIrrelevance::BST::{Leaf,Node};
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
    "{} | {} | {} | {} | {} | {} | {} | {} | | {} | {} | {} | {} | {} \
     | {:?} | {:?} | {:?} | {:?} | {} | {} | {} | {} | {} | {} | {:?} | {:?} | {:?}",
    // *** Identity ***
    Identity::id(42),
    Identity::idՖ10216Ֆ_Ֆ10217Ֆ_(42),
    Identity::idH(42),
    // Base types
    Numbers::add(40,2),
    Simplification::increment(41),
    // *** Recs ***
    Product::mapSnd(
      |x| x + 1
      , Product::_Ֆ215Ֆ_ {projՖ8321Ֆ: 0, projՖ8322Ֆ: 41}
    ).projՖ8322Ֆ,
    Product::projՖ8321Ֆ(Product::mapFst(
      |x| x + 1
      , Product::_Ֆ215Ֆ_ {projՖ8321Ֆ: 41, projՖ8322Ֆ: 40}
    )),
    Product::fst(Product::mapFst(
      |x| x + 1
      , Product::_Ֆ215Ֆ_ {projՖ8321Ֆ: 41, projՖ8322Ֆ: 40}
    )),
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
    // *** Erasure ***
    RunTimeIrrelevance::erasedFunArg(41),
    RunTimeIrrelevance::erasedHigherOrderFunArg(41),
    RunTimeIrrelevance::erasedRec,
    RunTimeIrrelevance::succ(RunTimeIrrelevance::ErasedField {x: 41}),
    RunTimeIrrelevance::erasedRecParam(RunTimeIrrelevance::ErasedRecParam {y: 42}),
    RunTimeIrrelevance::erasedData,
    RunTimeIrrelevance::erasedClause(RunTimeIrrelevance::ErasedCon::mk(42)),
    RunTimeIrrelevance::erasedConArg(RunTimeIrrelevance::ErasedConArg::mk(42)),
    Node(4, ᐁ(Node(2, ᐁ(Leaf()), ᐁ(Leaf()))), ᐁ(Leaf()))
    // *** Non-linear functions ***
    // double(21)
  );
}
