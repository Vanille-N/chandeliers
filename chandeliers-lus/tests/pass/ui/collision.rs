//! We are importing a library under a name that collides with
//! `chandeliers_sem` that the macro expands to. This checks
//! that we are properly using absolute paths in the macro.

#[allow(unused_imports)]
use rand as chandeliers_sem;

chandeliers_lus::decl! {
    node inc(x : int) returns (y : int);
    let y = x + 1; tel
}

fn main() {}
