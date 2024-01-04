//! No collision between several imports of the same extern variable.
const T: i64 = 0;

chandeliers_lus::decl! {
    extern const T: int;
}
chandeliers_lus::decl! {
    extern const T: int;
}
chandeliers_lus::decl! {
    extern const T: int;
}

fn main() {}
