//! Checking that Lustre (unlike Rust) allows shadowing a global variable
//! with a local one.
chandeliers_lus::decl! {
    const X : int = 0;
    node test() returns (X : bool);
    let X = 0; tel
}

fn main() {}
