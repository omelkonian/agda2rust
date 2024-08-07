#![feature(type_alias_impl_trait,impl_trait_in_fn_trait_return,unsized_locals,tuple_trait, unboxed_closures, fn_traits)]
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,incomplete_features,uncommon_codepoints, unused_imports)]

// ** Pointers
type ᐁ<A> = Box<A>;
// use std::rc::Rc;
// type ᐁ<A> = Rc<A>;
fn ᐁ<T>(x : T) -> ᐁ<T> { return ᐁ::new(x); }

// ** Lists
#[derive(Debug, Clone)]
enum List<A> {
  nil(),
  cons(A, ᐁ<List<A>>),
}

enum ListR<'a, A> {
  nil(),
  cons(A, &'a ListR<'a, A>),
}

fn cat<A>(xs: List<A>, ys: List<A>) -> List<A> {
  match xs {
    List::nil() => ys,
    List::cons(x, xs) => {
      let xs = *xs;
      List::cons(x, ᐁ(cat::<A>(xs, ys)))
    },
  }
}
fn catC<A: Clone>() -> impl Fn(List<A>) -> impl Fn(List<A>) -> List<A> {
  move |xs| move |ys| {
    let xs = xs.clone();
    // let ys = ys.clone();
    cat(xs, ys)
  }
}
fn cat1<A>() -> impl Fn(List<A>) -> impl FnOnce(List<A>) -> List<A> {
  move |xs| move |ys| {
    cat(xs, ys)
  }
}
fn catP<A>(xs: ᐁ<List<A>>, ys: ᐁ<List<A>>) -> ᐁ<List<A>> {
  let xs = *xs;
  let ys = *ys; 
  ᐁ(match xs {
    List::nil() => ys,
    List::cons(x, xs) => {
      let xs = *xs;
      List::cons(x, catP::<A>(ᐁ(xs), ᐁ(ys)))
    },
  })
}
fn catD<'a, A: Clone + 'a>() -> ᐁ<dyn Fn(List<A>) -> ᐁ<dyn Fn(List<A>) -> List<A> + 'a> + 'a> {
  ᐁ(move |xs| ᐁ(move |ys|
      cat(xs.clone(), ys)
  ))
}
fn catD2<'a, A: Clone + 'a>() -> ᐁ<dyn Fn(ᐁ<List<A>>) -> ᐁ<dyn Fn(ᐁ<List<A>>) -> ᐁ<List<A>> + 'a> + 'a> {
  ᐁ(move |xs| ᐁ(move |ys|
      catP(xs.clone(), ys)
  ))
}
fn catPC<'a, A : Clone + 'a>() -> ᐁ<dyn Fn(ᐁ<List<A>>) -> ᐁ<dyn Fn(ᐁ<List<A>>) -> ᐁ<List<A>> + 'a >> {
  ᐁ(move |xs| ᐁ(move |ys|
    catP(xs.clone(), ys)
  ))
}
// fn catR<'a, A>(xs: &'a ListR<A>, ys: &'a ListR<A>) -> &'a ListR<'a, A> {
//   // let xs = *(xs.clone());
//   // let ys = *ys; 
//   match xs {
//     ListR::nil() => ys,
//     ListR::cons(x, xs) => {
//       let x = *x;
//       let xs = *xs;
//       // panic!()
//       &ListR::cons(x, &catR(&xs, &ys))
//     },
//   }
// }
fn catCC<A: Clone>() -> impl Fn(List<A>) -> impl Fn(List<A>) -> List<A> {
  move |xs| move |ys|
    match xs.clone() {
      List::nil() => ys,
      List::cons(x, xs) => {
        let xs = *xs;
        List::cons(x, ᐁ(catCC::<A>()(xs)(ys)))
      },
    }
}

fn catDD<'a, A: Clone + 'a>() -> ᐁ<dyn Fn(List<A>) -> ᐁ<dyn Fn(List<A>) -> List<A> + 'a> + 'a> {
  ᐁ(move |xs| ᐁ(move |ys|
    match xs.clone() {
      List::nil() => ys,
      List::cons(x, xs) => {
        let xs = *xs;
        List::cons(x, ᐁ(catCC::<A>()(xs)(ys)))
      },
    }
  ))
}

fn map1<A, B, F>(f: F, xs: List<A>) -> List<B>
  where F: FnOnce(A) -> B + Clone
{
  match xs {
    List::nil() => List::nil(),
    List::cons(x1, xs) => {
      let xs = *xs;
      let f2 = f.clone();
      List::cons(f(x1), ᐁ(map1::<A, B, F>(f2, xs)))
    },
  }
}

fn map0<A, B>(f: impl Fn(A) -> B, xs: List<A>) -> List<B> {
  match xs {
    List::nil() => List::nil(),
    List::cons(x1, xs) => {
      let xs = *xs;
      List::cons(f(x1), ᐁ(map0::<A, B>(f, xs)))
    },
  }
}

fn map<A, B>(f: &dyn Fn(A) -> B, xs: List<A>) -> List<B> {
// fn map<A, B>(f: &impl Fn(A) -> B, xs: List<A>) -> List<B> {
  match xs {
    List::nil() => List::nil(),
    List::cons(x1, xs) => {
      let xs = *xs;
      List::cons(f(x1), ᐁ(map::<A, B>(f, xs)))
    },
  }
}

fn mapC<'a, A, B>() -> ᐁ<dyn Fn(&'a dyn Fn(A) -> B) -> ᐁ<dyn Fn(List<A>) -> List<B> + 'a>> {
    ᐁ(move |f| ᐁ(move |xs|
      map(f, xs))
    )
}

fn mapCC<'a, A, B>() -> impl Fn(&'a dyn Fn(A) -> B) -> ᐁ<dyn Fn(List<A>) -> List<B> + 'a> {
    move |f| ᐁ(move |xs|
      match xs {
        List::nil() => List::nil(),
        List::cons(x1, xs) => {
          let xs = *xs;
          List::cons(f(x1), ᐁ(mapCC::<A, B>()(f)(xs)))
        },
      }
    )
}

/*
// pub fn id<A>() -> impl Fn(A) -> A {
//   |x| x
// }

fn add(x: i32, y: i32) -> i32 {
  x + y
}

fn addC(x: i32) -> impl Fn(i32) -> i32 {
  move |y| add(x, y)
}

fn addCC() -> impl Fn(i32) -> impl Fn(i32) -> i32 {
  move |x| move |y| add(x, y)
}

fn id<A>(x: A) -> A {
  x
}

pub fn testIdId() -> i32 {
  // let idd: impl Fn(i32) -> i32 =
  // let idd: fn (_: i32) -> i32 =
  //   id(|x| id::<i32>(x));
  // idd(42)
  id(|x| id::<i32>(x))(42)
}

pub fn testIdAdd() -> i32 {
  // id(|x, y| x + y)(40, 2)
  id(|x| move |y| x + y)(40)(2)
}

pub fn main() {
  println!("{}:\t\t\t {} | {} | {} | {} | {}", module_path!(),
    testIdId(),
    testIdAdd(),
    add(40, 2),
    addC(40)(2),
    addCC()(40)(2),
  );
}
*/

/*
#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals,unreachable_patterns)]

pub struct ToNat<A, F: Fn(A) -> i32> {
  pub toNat: F,
  pub _phantom: std::marker::PhantomData<(A,)>,
}

pub fn ToNat·toNat<A, F: Fn(A) -> i32>(r: ToNat<A, F>, x: A) -> i32 {
  match r {
    ToNat { toNat, .. } => toNat(x),
  }
}
*/

/*
trait TyEq<T> {
  fn rw(self) -> T;
  fn rwi(x: T) -> Self;
}

impl<T> TyEq<T> for T {
  fn rw(self) -> T {
      self
  }
  fn rwi(x: T) -> Self {
      x
  }
}
*/

// * cutlass: doesn't work for `map` :(

// #[cutlass::curry]
// fn add_cc(x: u32, y: u32, z: u32) -> u32 {
//     return x + y + z;
// }

/*
#[cutlass::curry]
fn map_cc<A, B>(f: impl Fn(A) -> B, xs: List<A>) -> List<B> {
  match xs {
    List::nil() => List::nil(),
    List::cons(x1, xs) => {
      let xs = *xs;
      List::cons(f(x1), ᐁ(map_cc::<A, B>(f, xs)))
    },
  }
}
 */

// pub fn add(x: i32, y: i32) -> i32 {
//   x + y
// }
// pub fn addX(x: i32) -> impl Fn(i32) -> i32 {
//   move |y| x + y
// }
// pub fn addY() -> impl Fn(i32) -> impl Fn(i32) -> i32 {
//   move |x| move |y| x + y
// }

// * currying
use currying::*;

pub fn id<A>(x: A) -> A {
  x
}

pub fn idC<A>() -> impl Fn(A) -> A {
  move |x| x
}
// id(x1)(x2)(x3)

// pub fn main() {
//   let xs: List<i32> = List::cons(41, ᐁ(List::nil()));
//   let xs2 = xs.clone();
//   let xs3 = xs.clone();
//   let xs4 = xs.clone();
//   let ys: List<i32> = List::cons(42, ᐁ(List::nil()));
//   let ys2 = ys.clone();
//   let ys3 = ys.clone();
//   let ys4 = ys.clone();
//   let idNat: fn(i32) -> i32 = id::<i32>;
//   let id42: Curried<(i32,), (), (), _> = id::<i32>.curry(42);
//   let id42Call: i32 = id::<i32>.curry(42)();
//   let idId: Curried<(fn(i32) -> i32,), (), (), _> = id.curry(id::<i32>);
//   let idIdCall: fn(i32) -> i32 = id.curry(id::<i32>)();
//   let inc: fn(i32) -> i32 = |x| x + 1;
//   let mapInc: Curried<(fn(i32) -> i32,), (List<i32>,), (), _> = map0.curry(inc);
//   let mapIncXs: Curried<(List<i32>,), (), (), _> = map0.curry(inc).curry(xs3);
//   // let mapIncXsCall: List<i32> = map0.curry(inc).curry(xs4)();
//   println!("{}:\t\t {} | {} | {} | {} | {:?} | {:?} | {:?} | {:?}", module_path!(),
//     id.curry(id)().curry(42)(),
//     id.curry(id)()(42),
//     id.curry(id)().curry(id)().curry(42)(),
//     id.curry(id)().curry(id)()(42),
//     map0.curry(|x| x + 1).curry(xs)(),
//     map0.curry(|x| x + 1)(xs2),
//     cat.curry(ys).curry(ys2)(),
//     cat.curry(ys3)(ys4),
//   );
// }
