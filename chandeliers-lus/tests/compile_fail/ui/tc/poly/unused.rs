//! Unused type variable on node.
//! This is bad because we can't instanciate it.
chandeliers_lus::decl! {
    #[generic[T]]
    node id(t0: int) returns (t1: int);
    let t1 = t0; tel;
}

fn main() {}
