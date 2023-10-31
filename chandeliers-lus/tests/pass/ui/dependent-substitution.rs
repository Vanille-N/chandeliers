chandeliers_lus::decl! {
    node foo(b1 : bool) returns (i : int when b1);
    let
        i = 1 when b1;
    tel;

    node system1() returns ();
    var b1, b2 : bool;
        n : int when b2;
    let
        b1 = true; b2 = true;
        n = foo(b2);
    tel;
}

chandeliers_lus::decl! {
    node bar(b1 : bool; j : int whenot b1) returns (i : int);
    let
        i = merge b1 0 j;
    tel;

    node system2() returns ();
    var b1, b2 : bool;
        n : int;
    let
        b1 = true; b2 = true;
        n = bar(b2, 1 whenot b2);
    tel;
}

chandeliers_lus::decl! {
    node baz(b1, b2 : bool; i : int when b1) returns ();
    let tel;

    node system3() returns ();
    var b1, b2 : bool;
    let
        b1 = true; b2 = true;
        () = baz(b2, b1, 1 when b2);
    tel;
}

fn main() {}
