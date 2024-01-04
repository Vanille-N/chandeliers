//! Checking that both errors are reported.
chandeliers_lus::decl! {
    const X : int = (true + true) + (false + false);
}

fn main() {}
