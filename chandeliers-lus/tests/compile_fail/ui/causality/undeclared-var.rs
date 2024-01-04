//! Undeclared variable in node.
//! This should be caught by chandeliers, not rustc.
chandeliers_lus::decl! {
    node foo() returns (i: int);
    let i = absent; tel;
}

fn main() {}
