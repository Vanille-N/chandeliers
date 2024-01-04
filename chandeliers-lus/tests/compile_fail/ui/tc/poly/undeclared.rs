//! Undeclared type variable on node.
chandeliers_lus::decl! {
    node id(t0: T) returns (t1: T);
    let t1 = t0; tel;
}

fn main() {}
