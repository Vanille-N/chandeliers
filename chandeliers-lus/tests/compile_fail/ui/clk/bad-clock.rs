chandeliers_lus::decl! {
    node ignore() returns ();
    let tel;

    node b() returns (b : bool);
    let b = true; tel;

    node foo() returns ();
    let
        () = ignore(() when true);
        () = ignore(() when (42 = 12));
        () = ignore(() when (b()));
    tel;
}

fn main() {}
