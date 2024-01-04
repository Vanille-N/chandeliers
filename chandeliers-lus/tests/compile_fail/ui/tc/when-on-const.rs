//! Can't use `when` on a constant.
chandeliers_lus::decl! {
    const X : int when b = 0;
}

fn main() {}
