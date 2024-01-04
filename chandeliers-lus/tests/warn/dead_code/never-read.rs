//! Checking that dead code analysis applies to assigned but unused variables.
#![deny(warnings)]

chandeliers_lus::decl! {
    node foo(x : int) returns (y : int);
    var z : int;
    let y = x; z = 1; tel;
}

fn main() {}
