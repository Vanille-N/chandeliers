//! A collection of binary operators with wrong type operands.
chandeliers_lus::decl! {
    const B : bool = true + true;
}

chandeliers_lus::decl! {
    const R : float = 0.1 % 0.5;
}

chandeliers_lus::decl! {
    const F : float = 0.1 and 0.4;
}

fn main() {}
