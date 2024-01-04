//! There are several typing errors in the same tuple here, we check that they are all
//! reported not just the first.
chandeliers_lus::decl! {
    node foo(x, y, z : int) returns (a, b, c : bool);
    let (a, b, c) = (x, y, z); tel
}

fn main() {}
