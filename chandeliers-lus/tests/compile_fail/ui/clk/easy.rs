chandeliers_lus::decl! {
    node foo(b1, b2 : bool; i : bool when b1) returns (o : bool when b2);
    let o = true whenot b1; tel;
}

chandeliers_lus::decl! {
    node foo(b1, b2 : bool; i : bool when b1) returns (o : bool when b2);
    let o = true when b1; tel;
}

chandeliers_lus::decl! {
    node foo(b1, b2 : bool; i : bool when b1) returns (o : bool when b2);
    let o = (true when b2) or (true when b1); tel;
}

chandeliers_lus::decl! {
    node foo() returns (o1 : bool; o2 : bool when o1);
    let o1 = true; o2 = true when o1; tel;

    node bar() returns (o1 : bool; o2 : bool when o1);
    let (o1, o2) = foo(); tel;
}

chandeliers_lus::decl! {
    node foo(b1, b2 : bool) returns (i : int when b1);
    let i = 1 when b1; tel;

    node bar(b1, b2 : bool) returns (i : int when b1);
    let i = foo(b2, b1); tel;
}

chandeliers_lus::decl! {
    node foo(b1 : bool; b2 : bool when b1) returns ();
    let tel;

    node bar(b1 : bool) returns ();
    let () = foo(b1, true when b1); tel
}

fn main() {}
