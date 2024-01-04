//! Undeclared variable used as clock.
chandeliers_lus::decl! {
    node foo(x : int when b) returns ();
    let tel;
}

chandeliers_lus::decl! {
    node foo(x : int when b) returns (b : bool);
    let b = true; tel;
}

fn main() {}
