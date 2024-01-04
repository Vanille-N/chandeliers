//! Can't do equality tests or binary operations on generic type.
chandeliers_lus::decl! {
    #[generic[T]]
    node eq(t, u: T) returns (b : bool);
    let b = (t = u); tel;
}

chandeliers_lus::decl! {
    #[generic[T]]
    node plus(t, u: T) returns (out: T);
    let out = t + u; tel;
}

fn main() {}
