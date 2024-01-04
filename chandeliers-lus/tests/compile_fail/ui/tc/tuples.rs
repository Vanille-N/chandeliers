//! Unpacking tuple leads to wrong types.
chandeliers_lus::decl! {
    node gen() returns (a : int; b : float; c : bool);
    let
        a = 1;
        b = 0.5;
        c = true;
    tel;

    node main() returns ();
    var a, b, c : int;
    let
        (a, b, c) = gen();
    tel
}

fn main() {}
