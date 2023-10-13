#![allow(unused_variables)]

chandeliers_lus::decl! {
    node empty_all() returns ();
    let tel;

    node empty_with_var() returns ();
    var x:bool;
    let x = true; tel;
}

fn main() {}
