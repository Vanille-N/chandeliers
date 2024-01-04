//! Two node parameters with the same name.
//! This would be caught by rustc but we might as well get a nicer error message
//! early because several stages of sanitization would not like this.
chandeliers_lus::decl! {
    node foo(a, a : int) returns ();
    let tel
}

fn main() {}
