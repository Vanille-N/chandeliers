chandeliers_lus::decl! {
    node foo(b : bool) returns (i : int when b);
    let i = 1 when b; tel;

    node bar(b : bool) returns (i : int whenot b);
    let i = 2 whenot b; tel;

    #[main(10)]
    node system() returns ();
    var b : bool;
        i : int;
    let
        b = true fby not b;
        i = merge b (foo(b)) (bar(b));
        assert i = 1 fby 3 - i;
    tel;
}
