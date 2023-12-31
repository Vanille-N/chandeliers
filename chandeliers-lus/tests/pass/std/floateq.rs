//! How to properly do an "equality" test on floats.
chandeliers_lus::decl! {
    node fabs(i : float) returns (o : float);
    let
        o = if i > 0.0 then i else -i;
    tel;

    #[export]
    node rounding_error() returns ();
    let
        assert fabs(0.1 + 0.2 - 0.3) < 0.001;
    tel
}

fn main() {
    use chandeliers_sem::traits::*;
    let mut r = rounding_error::default();
    r.step(().embed());
}
