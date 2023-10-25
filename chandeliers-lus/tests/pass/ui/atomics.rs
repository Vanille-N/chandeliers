use chandeliers_std::cast::float_of_int;

chandeliers_lus::decl! {
    extern node float_of_int(i : int) returns (f : float);

    node foo() returns ();
    var t : bool;
    let
        t = not pre not not pre (pre not not true or not not (pre float_of_int(1) > pre - pre pre - - - - 0.0));
    tel
}

fn main() {}
