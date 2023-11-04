const WRONG_TY: i64 = 0;
chandeliers_lus::decl! {
    extern const WRONG_TY: float;

    node foo() returns (f : float);
    let f = WRONG_TY; tel
}

fn main() {}
