//! Undeclared variable.
//! This should be caught at the chandeliers level, if rustc
//! is the one to flag it the error message will be worse.
chandeliers_lus::decl! {
    const B: int = A;
}

fn main() {}
