chandeliers_lus::decl! {
    extern node float_of_int(i : int) returns (f : float);

    node main() returns ();
    var x : float;
    let
        x = float_of_int(1, 2);
    tel
}

fn main() {}
