open import Agda.Builtin.Nat using (Nat; _+_)

answer : Nat
answer = 42

suc : Nat → Nat
suc x = x + 1

add_answer : Nat → Nat
add_answer x = x + answer

add : Nat → Nat → Nat
add x y = x + y

add3 : Nat → Nat → Nat → Nat
add3 x y z = x + y + z

add3b : Nat → Nat → Nat → Nat
add3b x y z = add x (add y z)

{-# FOREIGN AGDA2RUST
pub fn main() {
  println!("{}:\t\t {} | {} | {} | {} | {} | {}", module_path!(),
    answer(),
    suc(41),
    add_answer(0),
    add(40, 2),
    add3(40, 1, 1),
    add3b(40, 1, 1),
  );
}
#-}
