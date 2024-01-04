//! Wrong output type.
chandeliers_lus::decl! {
    extern node float_of_int(i : int) returns (f : float);

    node main() returns ();
    var x, y : float;
    let
        (x, y) = float_of_int(1);
    tel
}

fn main() {}
