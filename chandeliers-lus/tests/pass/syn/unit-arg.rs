chandeliers_lus::decl! {
    node foo() returns ();
    let
        () = ((()));
        ((),) = (((()),));
        ((())) = ();
    tel;

    node bar() returns ();
    let
        () = foo((()));
    tel
}

fn main() {}
