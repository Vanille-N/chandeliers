const E: i64 = 42;

chandeliers_lus::decl! {
    extern const E : int;
    #[export]
    const B : bool = (true = false) or not (true = false);
    #[export]
    const X : int = if B then 5 else 42;
    #[export]
    const Y : int = X + (if not not not B then 0 else 1);
    #[export]
    const Z : bool = (2 * X * Y) <= 100;
    #[export]
    const E2 : int = E;
}

fn main() {
    assert_eq!(B, true);
    assert_eq!(X, 5);
    assert_eq!(Y, 6);
    assert_eq!(Z, true);
    assert_eq!(E2, 42);
}
