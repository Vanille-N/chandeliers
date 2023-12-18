chandeliers_lus::decl! {
    node id_int(i0: int) returns (i1: int);
    let i1 = i0; tel;

    #[generic[T]]
    node id(t0: T) returns (t1: T);
    let t1 = id_int(t0); tel;
}

fn main() {}
