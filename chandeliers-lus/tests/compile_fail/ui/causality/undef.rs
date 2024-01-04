//! Missing a definition.
//! If rustc is made to catch this the error will be atrocious, so we
//! really want chandeliers to flag it.
chandeliers_lus::decl! {
    node foo() returns (i : int);
    let tel;
}

fn main() {}
