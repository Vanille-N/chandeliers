chandeliers_lus::decl! {
    #[main(1)]
    node system() returns ();
    var ok : bool;
    let
        ok = true -> false;
        assert ok;
    tel;
}
