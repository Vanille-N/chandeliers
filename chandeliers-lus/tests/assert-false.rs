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
