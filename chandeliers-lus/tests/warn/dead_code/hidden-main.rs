#![deny(dead_code)]

mod hidden {
    chandeliers_lus::decl! {
        #[main]
        node foo() returns ();
        let tel;
    }
}

fn main() {}
