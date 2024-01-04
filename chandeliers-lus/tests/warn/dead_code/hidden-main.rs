//! Check that `#[main]` doesn't interfere with dead code analysis.
#![deny(dead_code)]

mod hidden {
    chandeliers_lus::decl! {
        #[main]
        node foo() returns ();
        let tel;
    }
}

fn main() {}
