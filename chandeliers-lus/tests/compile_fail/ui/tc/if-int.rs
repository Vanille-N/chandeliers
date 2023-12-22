chandeliers_lus::decl! {
    node foo() returns ();
    let
        assert if 1 then true else false;
    tel;
}

chandeliers_lus::decl! {
    node foo() returns ();
    let
        assert if (1, 2, 3) then true else false;
    tel;
}

chandeliers_lus::decl! {
    node foo() returns ();
    let
        assert 1;
    tel;
}

chandeliers_lus::decl! {
    node foo() returns ();
    let
        assert (1, 2, 3);
    tel;
}

fn main() {}
