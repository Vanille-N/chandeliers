//! This can't be an `if`, it only works as a `merge`.
chandeliers_lus::decl! {
    node foo(b : bool; x, y : int) returns (i : int);
    let
        i = if b then x when b else y whenot b;
    tel;
}

fn main() {}
