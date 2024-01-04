//! This time the missing constant has to be caught by rustc.
//! We can try to get as good an error message as possible.
chandeliers_lus::decl! {
    extern const MISSING: float;

    node foo() returns (f : float);
    let
        f = MISSING;
    tel;
}

fn main() {}
