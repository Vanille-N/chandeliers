//! Same variable is part of the inputs and the locals.
//! To rustc this just looks like a shadowing so we must catch it early.
chandeliers_lus::decl! {
    node foo(a : int) returns ();
    var a : float;
    let a = 0.0; tel
}

fn main() {}
