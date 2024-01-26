import Identity
import Numbers
import Maybe
import Either
-- import Exp
-- import List
-- import Product
-- import Lambdas

{-# FOREIGN AGDA2RUST
mod Identity;
mod Numbers;
mod Maybe;
mod Either;
// mod Exp;
// mod List;
// mod Product;
// mod Lambdas;

fn main() {
  println!("{} {}", Identity::id("Hi!"), Numbers::add(40,2));
}
#-}
