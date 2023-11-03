chandeliers_lus::decl! {
    node foo(b1 : bool) returns (i : int when b1);
    let
        i = 1 when b1;
    tel;

    node system1() returns ();
    var b1, _b2 : bool;
        _n : int when _b2;
    let
        b1 = true; _b2 = true;
        _n = foo(b1);
    tel;
}

chandeliers_lus::decl! {
    node bar(b1 : bool; j : int whenot b1) returns (i : int);
    let
        i = merge b1 2 j;
    tel;

    node system2() returns ();
    var b1, b2 : bool;
        _n : int;
    let
        b1 = true; b2 = true;
        _n = bar(b2, 1 whenot b1);
    tel;
}

chandeliers_lus::decl! {
    node baz(_b1, _b2 : bool; _i : int when _b1) returns ();
    let tel;

    node system3() returns ();
    var b1, b2 : bool;
    let
        b1 = true; b2 = true;
        () = baz(b2, b1, 1 when b1);
    tel;
}

fn main() {}
