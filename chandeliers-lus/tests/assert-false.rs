//! Manual test (the test framework supports only `compile-fail`, not `fail`
//! tests) to check that `assert`ing a false value properly crashes the program.
//! The correctness of this test is the foundation for all other tests where
//! we rely only on inline `assert`s to verify proper execution.

chandeliers_lus::decl! {
    #[export]
    node False() returns ();
    let
        assert false;
    tel
}

#[test]
#[should_panic = "Assertion failed: false"]
fn main() {
    use chandeliers_san::candle::traits::*;
    let mut f = False::default();
    f.step(().embed()).trusted();
}
