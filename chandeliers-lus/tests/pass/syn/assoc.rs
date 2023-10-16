chandeliers_lus::decl! {
    node assoc() returns ();
    let
        assert 5 - 1 - 1 - 1 = 2;
        assert 5 - 1 + 2 = 6;
        assert 8 / 2 / 2 = 2;
        assert 8 / 2 * 4 = 16;
        assert 1 * 2 + 1 * 2 = 4;
        assert --pre --4 = 4;
    tel
}

fn main() {
    let mut a = assoc::default();
    a.update_mut();
}
