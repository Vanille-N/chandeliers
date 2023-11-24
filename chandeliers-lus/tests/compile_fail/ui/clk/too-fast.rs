chandeliers_lus::decl! {
    node system() returns ();
    var b : bool;
        n : int when b;
    let
        b = true fby not b;
        n = if b then 1 else 2;
    tel
}

fn main() {}
