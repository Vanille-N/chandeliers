//! Unused type variable on extern node.
//! This is bad because we can't instanciate it.
chandeliers_lus::decl! {
    #[generic[T]]
    extern node id(t0: int) returns (t1: int);
}

fn main() {}
