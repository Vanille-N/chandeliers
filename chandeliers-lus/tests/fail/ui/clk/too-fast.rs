chandeliers_lus::decl! {
    node system() returns ();
    var b : bool; _n : int when b;
    let
        b = true fby not b;
        _n = 1;
    tel
}

fn main() {}
