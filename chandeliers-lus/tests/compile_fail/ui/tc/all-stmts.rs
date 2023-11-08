chandeliers_lus::decl! {
    node foo() returns ();
    let
        assert 1 = true;
        assert (1 + true) = (2 + true);
        assert 0.1 = 0.2;
    tel
}

fn main() {}
