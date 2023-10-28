chandeliers_lus::decl! {
    #[trace]
    node cumul(i : int) returns (s : int);
    let
        s = i + (0 fby s);
    tel;
}

chandeliers_lus::decl! {
    #[trace]
    extern node cumul(i : int) returns (s : int);

    #[main]
    node system() returns ();
    var i : int;
    let
        i = cumul(1 fby i);
        assert i < 1000;
    tel;
}
