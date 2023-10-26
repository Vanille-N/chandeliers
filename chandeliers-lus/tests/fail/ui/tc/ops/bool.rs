chandeliers_lus::decl! {
    node foo(i : int) returns (b : bool);
    let
        b = if i then 0 else 1;
    tel
}

chandeliers_lus::decl! {
    node bar(i : int) returns ();
    let
        assert i;
    tel
}

fn main() {}
