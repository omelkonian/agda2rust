#![allow(dead_code,non_snake_case,unused_variables,non_camel_case_types,non_upper_case_globals)]

pub enum Maybe<  A,> {
  just(A),
  nothing(),
}

