//! These operators are not associative.
chandeliers_lus::decl! {
    const B: bool = 0 < 1 > 2 < 3;
}

chandeliers_lus::decl! {
    const B2: bool = 0 = 1 <> 2 = 3;
}

fn main() {}
