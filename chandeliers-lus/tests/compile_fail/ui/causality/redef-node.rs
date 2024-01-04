//! Duplicate node definitions.
chandeliers_lus::decl! {
    node foo() returns ();
    let tel;

    node foo() returns ();
    let tel;
}

fn main() {}
