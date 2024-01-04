//! Node exists but isn't properly implemented.
#[allow(non_camel_case_types)]
#[derive(Debug)]
struct no_default {}

chandeliers_lus::decl! {
    extern node no_default() returns ();
}

fn main() {}
