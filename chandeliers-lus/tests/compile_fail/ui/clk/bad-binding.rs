chandeliers_lus::decl! {
    extern node random_bool() returns (b : bool);

    node system() returns ();
    var b1, b2 : bool;
        _n : int when b1;
    let
        b1 = random_bool();
        b2 = random_bool();
        _n = 1 when b2;
    tel
}

fn main() {}
