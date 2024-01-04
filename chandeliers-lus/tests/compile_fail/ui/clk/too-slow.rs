//! `_n` is not initialized enough.
chandeliers_lus::decl! {
    node system() returns ();
    var b : bool; _n : int;
    let
        b = true fby not b;
        _n = 1 when b;
    tel
}

fn main() {}
