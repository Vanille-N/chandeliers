#![deny(dead_code)]

mod hidden {
    chandeliers_lus::decl! {
        #[export]
        node foo() returns ();
        let tel;
    }
}

fn main() {}
