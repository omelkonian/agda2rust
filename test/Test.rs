// *** module Test ***


fn answer() -> i32 {
  42
}
fn suc(x: i32) -> i32 {
  x + 1
}
fn addAnswer(x: i32) -> i32 {
  x + answer()
}
fn add(x: i32, y: i32) -> i32 {
  x + y
}
fn add3(x: i32, y: i32, z: i32) -> i32 {
  x + y + z
}
fn add3b(x: i32, y: i32, z: i32) -> i32 {
  add(x, add(y, z))
}
enum Exp<  v,> {
  Plus(Exp<v>, Exp<v>),
  Int(i32),
  Var(v),
}



