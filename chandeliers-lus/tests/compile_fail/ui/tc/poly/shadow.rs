//! Can't use a type variable of a preexisting type.
chandeliers_lus::decl! {
    #[generic[int]]
    node foo(i: int) returns (o: int);
    let o = i; tel;
}

fn main() {}
