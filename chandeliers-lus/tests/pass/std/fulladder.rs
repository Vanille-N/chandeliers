chandeliers_lus::decl! {
    node full_add(a, b, c : bool) returns (s, co : bool);
    let
        s = (a <> b) <> c;
        co = (a and b) or (a and c) or (b and c);
    tel;

    node half_add(a, b : bool) returns (s, co : bool);
    let
        s = (a <> b);
        co = (a and b);
    tel;

    node full_add_h(a, b, c : bool) returns (s, co : bool);
    var s1, c1, c2 : bool;
    let
        (s1, c1) = half_add(a, b);
        (s, c2) = half_add(c, s1);
        co = (c1 or c2);
    tel;

    node equivalence(a, b, c : bool) returns (ok : bool);
    var s1, co1, s2, co2: bool;
    let
        (s1, co1) = full_add(a, b, c);
        (s2, co2) = full_add_h(a, b, c);
        ok = (s1 = s2) and (co1 = co2);
    tel;

    #[main(8)]
    node system() returns ();
    var a, b, c, ok : bool; enumerate : int;
    let
        enumerate = 0 fby enumerate + 1;

        a = (enumerate and 1) > 0;
        b = (enumerate and 2) > 0;
        c = (enumerate and 4) > 0;

        ok = equivalence(a, b, c);
        assert ok;
    tel;
}
