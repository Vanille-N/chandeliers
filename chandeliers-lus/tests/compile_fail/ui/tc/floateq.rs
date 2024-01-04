//! `f64` famously doesn't implement `Eq`, and for good reason.
//! We keep this behavior here.
chandeliers_lus::decl! {
    node rounding_error() returns ();
    let
        assert 0.1 + 0.2 = 0.3;
    tel
}

fn main() {}
