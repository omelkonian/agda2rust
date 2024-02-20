import Identity
import Numbers
import Maybe
import Either
import List0
import Product
import Exp
-- import List
-- import Lambdas

{-# FOREIGN AGDA2RUST
mod Identity;
mod Numbers;
mod Maybe; use Maybe::Maybe::{Just};
mod Either; use Either::Either::{Left};
mod List0; use List0::List::{Cons,Nil};
mod Product;
mod Exp; use Exp::Exp::{Plus,Int,Var};
// mod List;
// mod Lambdas;

fn main() {
  println!("{} | {} | {} | {} | {:?} | {} | {}",
    Identity::id("Hi!"),
    Numbers::add(40,2),
    Maybe::fromMaybe(41, Just(42)),
    Either::fromEither::<i32, i32>(41, Left(42)),
    List0::map(|x| x + 1, List0::con(
      Cons(3, Box::new(Nil())),
      Cons(1, Box::new(Nil()))
    )),
    Product::mapSnd(|x| x + 1, Product::Product {fst: 0, snd: 41}).snd,
    Exp::eval(|x| x, Plus(Box::new(Int(5)),Box::new(Var(37))))
  );
}
#-}
