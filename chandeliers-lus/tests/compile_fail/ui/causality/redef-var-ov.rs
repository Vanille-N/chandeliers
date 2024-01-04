//! Duplicate variable definition.
//! For rustc this is just a variable shadowing so we need to catch it manually.
chandeliers_lus::decl! {
    node foo() returns (a : bool);
    var a : int;
    let a = 0; tel
}

fn main() {}
