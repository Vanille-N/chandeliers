//! Trying no name variables with reserved keywords.
chandeliers_lus::decl! {
    const crate : int = 0;
}

chandeliers_lus::decl! {
    const self : int = 0;
}

chandeliers_lus::decl! {
    const Self : int = 0;
}

chandeliers_lus::decl! {
    node super() returns ();
    let tel
}

chandeliers_lus::decl! {
    node move() returns ();
    let tel
}

chandeliers_lus::decl! {
    node static() returns ();
    let tel
}

fn main() {}
