//! Duplicate definition of output variable.
//! For rustc this is just a variable shadowing so we need to catch it
//! manually.
chandeliers_lus::decl! {
    node foo() returns (a : bool; a : int);
    let a = 1; tel
}

fn main() {}
