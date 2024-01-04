//! Duplicate local variable definition.
//! This *must* be caught by chandeliers because rustc would just
//! see a variable shadowing which is ok.
chandeliers_lus::decl! {
    node foo() returns (a : int);
    let
        a = 42;
        a = 7;
    tel;
}

fn main() {}
