//! Unary operators with wrong typed operands.
chandeliers_lus::decl! {
    const X : float = not 4.0;
}

chandeliers_lus::decl! {
    const B : bool = -true;
}

fn main() {}
