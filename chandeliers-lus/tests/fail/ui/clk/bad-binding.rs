use chandeliers_std::rand::random_bool;

chandeliers_lus::decl! {
    extern node random_bool() returns (b : bool);

    node system() returns ();
    var _b1, b2 : bool;
        _n : int when _b1;
    let
        _b1 = random_bool();
        b2 = random_bool();
        _n = 1 when b2;
    tel
}

fn main() {}
