//! Thanks to proper name sanitization, there's no issue with having a variable
//! of the node have the same name as the node itself.
chandeliers_lus::decl! {
    #[export]
    const X : int = 0;

    #[export]
    node X() returns (X : int);
    let X = 1; tel
}

fn main() {
    use chandeliers_sem::traits::*;
    assert_eq!(X, 0);
    let mut x = X::default();
    assert_eq!(x.step(().embed()).trusted(), 1);
}
