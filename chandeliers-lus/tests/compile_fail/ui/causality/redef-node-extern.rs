//! Duplicate definition of an `extern node`. It
//! is not possible for chandeliers to catch this, so
//! here we are verifying that rustc properly catches it
//! and puts the right spans.
chandeliers_lus::decl! {
    extern node foo(i : int) returns (i : int);
}

chandeliers_lus::decl! {
    extern node bar(i : int; i : bool) returns ();
}

fn main() {}
