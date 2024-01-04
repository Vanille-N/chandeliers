//! Duplicate definitions.
//! Known issue: there's some duplication of rustc errors that isn't great.
chandeliers_lus::decl! {
    #[export]
    const X: int = 0;
}

chandeliers_lus::decl! {
    #[export]
    const X: int = 0;
}

chandeliers_lus::decl! {
    #[export]
    node foo() returns ();
    let tel
}

chandeliers_lus::decl! {
    #[export]
    node foo() returns ();
    let tel
}

fn main() {}
