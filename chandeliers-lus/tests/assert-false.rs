chandeliers_lus::decl! {
    node False() returns ();
    let
        assert false;
    tel
}

#[test]
#[should_panic = "Assertion failed: false"]
fn main() {
    use chandeliers_sem::traits::*;
    let mut f = False::default();
    f.step(())
}
