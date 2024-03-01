#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types)]
#[derive(Debug)]
pub enum The<  A,> {
  the(A),
  _Impossible(std::marker::PhantomData<(A,)>),
}

