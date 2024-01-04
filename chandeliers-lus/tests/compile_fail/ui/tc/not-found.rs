//! Constant or local variable not found.
chandeliers_lus::decl! {
    const Y : int = 42;
    const Z : int = 12;
    node foo() returns (i : int);
    let
        i = X;
    tel
}

fn main() {}
