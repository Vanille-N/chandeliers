//! Checking that `#[export]` connects with rustc's dead code analysis.
#![deny(dead_code)]

mod hidden {
    chandeliers_lus::decl! {
        #[export]
        node foo() returns ();
        let tel;

        #[export]
        const FOO: int = 0;
    }
}

fn main() {}
