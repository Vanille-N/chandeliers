chandeliers_lus::decl! {
    node count() returns (n : int);
    let n = 0 fby n + 1; tel;

    node a() returns (i, j : int);
    let i = 0; j = count(); tel;
    node b() returns (i, j : int);
    let i = 1; j = count(); tel;
    node c() returns (i, j : int);
    let i = 2; j = count(); tel;

    #[trace]
    node testing() returns (i, j : int);
    let
        (i, j) = a() fby b() fby c();
    tel;

    node witness() returns (i, j : int);
    let
        (i, j) =
            (0, 0) ->
            (1, 0) ->
            (2, 0) ->
            (2, 1) ->
            (2, 2) ->
            (2, 3) ->
            (2, 4);
    tel;

    #[main(7)]
    node system() returns ();
    var ti, tj, wi, wj : int;
    let
        (ti, tj) = testing();
        (wi, wj) = witness();
        assert ti = wi;
        assert tj = wj;
    tel;
}
