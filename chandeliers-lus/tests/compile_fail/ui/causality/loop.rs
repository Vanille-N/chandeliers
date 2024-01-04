//! All-in-one cyclic definitions.
chandeliers_lus::decl! {
    const X: int = X + 1;
}

chandeliers_lus::decl! {
    node foo() returns (i : int);
    let
        i = foo();
    tel
}

chandeliers_lus::decl! {
    node bar() returns (f : float);
    let
        f = 1.0 + f * 0.5;
    tel;
}

chandeliers_lus::decl! {
    node foo() returns ();
    let () = bar(); tel;

    node bar() returns ();
    let () = foo(); tel;
}

fn main() {}
