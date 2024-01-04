//! Checking the topological sort on node declarations.
chandeliers_lus::decl! {
    node f() returns ();
    let () = g(); tel;

    node g() returns ();
    let () = h(); tel;

    node h() returns ();
    let tel;
}

fn main() {}
