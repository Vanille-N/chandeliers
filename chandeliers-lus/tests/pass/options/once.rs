//! Controlling the number of executions so that it doesn't fail
//! (it would on the second iteration).
chandeliers_lus::decl! {
    #[main(1)]
    node system() returns ();
    var ok : bool;
    let
        ok = true -> false;
        assert ok;
    tel;
}
