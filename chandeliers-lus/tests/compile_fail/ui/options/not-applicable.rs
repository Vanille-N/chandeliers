//! Annotation on a construct that doesn't accept them.
//! (`#[main]` is only for `node`)
chandeliers_lus::decl! {
    #[main]
    const X : int = 0;
}

fn main() {}
