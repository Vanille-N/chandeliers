//! Dead code analysis for extern imports.
#![deny(dead_code)]

chandeliers_lus::decl! {
    #[export]
    const X : int = 0;

    #[export]
    node foo() returns ();
    let tel;
}

chandeliers_lus::decl! {
    #[rustc_allow[dead_code]]
    extern const X : int;
    #[rustc_allow[dead_code]]
    extern node foo() returns ();
}

chandeliers_lus::decl! {
    extern const X : int;
    extern node foo() returns ();
}

fn main() {}
