chandeliers_lus::decl! {
    node fabs(i : float) returns (o : float);
    let
        o = if i > 0.0 then i else -i;
    tel;

    node rounding_error() returns ();
    let
        assert fabs(0.1 + 0.2 - 0.3) < 0.001;
    tel
}

fn main() {
    let mut r = rounding_error::default();
    r.step(());
}
