//! Variable is part of both inputs and outputs. To
//! rustc this looks like just a variable shadowing, so we must reject it
//! early.
chandeliers_lus::decl! {
    node foo(a : int) returns (a : bool);
    let a = true; tel
}

fn main() {}
