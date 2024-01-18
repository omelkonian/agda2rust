// enum Message {
//     Quit,
//     Move { x: i32, y: i32 },
//     Write(String),
//     ChangeColor(i32, i32, i32),
// }

// enum TupleVariants {
//     None(),
//     One(i32),
//     Two(i32,i32)
// }

// enum Option<T> {
//     None,
//     Some(T),
// }

enum List<T> {
    Nil,
    Cons(T, List<T>),
}
