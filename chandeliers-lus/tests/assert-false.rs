chandeliers_lus::decl! {
    node False() returns ();
    let
        assert false;
    tel
}

#[test]
#[should_panic = "Assertion failed: false"]
fn main() {
    let mut f = False::default();
    f.update_mut()
}
