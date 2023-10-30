chandeliers_lus::decl! {
    node system() returns ();
    var b : bool; n : int;
    let
        b = true fby not b;
        n = 1 when b;
    tel
}

fn main() {}
