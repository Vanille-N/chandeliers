chandeliers_lus::decl! {
    extern const MISSING: float;

    node foo() returns (f : float);
    let
        f = MISSING;
    tel;
}

fn main() {}
