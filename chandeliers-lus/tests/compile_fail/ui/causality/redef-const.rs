//! Duplicate definition of a const.
chandeliers_lus::decl! {
    const X : int = 0;
    const X : float = 0.0;
}

fn main() {}
