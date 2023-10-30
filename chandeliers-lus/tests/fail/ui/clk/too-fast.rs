chandeliers_lus::decl! {
    node system() returns ();
    var b : bool; n : int when b;
    let
        b = true fby not b;
        n = 1;
    tel
}

fn main() {}
