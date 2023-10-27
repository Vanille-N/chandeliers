chandeliers_lus::decl! {
    const X : int = 0;
    node X() returns (X : int);
    let X = 1; tel
}

fn main() {
    use chandeliers_sem::traits::*;
    assert_eq!(X, 0);
    let mut x = X::default();
    assert_eq!(x.step(()).unwrap(), 1);
}
