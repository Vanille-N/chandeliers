//! There are several errors here, we are checking that they *all* get
//! reported, not just the first one.
chandeliers_lus::decl! {
    node foo() returns ();
    let
        assert 1 = true;
        assert (1 + true) = (2 + true);
        assert 0.1 = 0.2;
    tel
}

fn main() {}
