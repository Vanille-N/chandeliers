//! Trailing comma makes `((),)` not the same type as (())`.
chandeliers_lus::decl! {
    node foo() returns ();
    let
        () = ((),);
    tel;
}

chandeliers_lus::decl! {
    node foo() returns ();
    let tel;

    node bar() returns ();
    let () = foo((),); tel;
}

fn main() {}
