//! Missing node has to be caught by rustc.
chandeliers_lus::decl! {
    extern node missing() returns ();
}

fn main() {}
