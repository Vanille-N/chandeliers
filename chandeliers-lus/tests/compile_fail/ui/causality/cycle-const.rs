//! Cyclic definition of `const` values.
chandeliers_lus::decl! {
    const A : int = B + C;
    const C : int = 1;
    const B : int = D;
    const D : int = A;
}

fn main() {}
