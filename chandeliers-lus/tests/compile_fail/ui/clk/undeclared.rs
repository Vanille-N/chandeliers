//! `when` uses an undeclared variable as clock.
chandeliers_lus::decl! {
    node foo(b : bool; i : int when b2) returns ();
    let tel;
}

fn main() {}
