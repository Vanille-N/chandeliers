chandeliers_lus::decl! {
    #[main]
    node system() returns ();
    var n : int;
    let
        n = (0 fby n + 1) % 10;
        assert n < 10;
    tel
}
