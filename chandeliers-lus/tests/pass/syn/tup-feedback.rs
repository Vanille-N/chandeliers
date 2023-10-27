use chandeliers_sem::traits::*;

chandeliers_lus::decl! {
    node swap(inl, inr : int) returns (outl, outr : int);
    let
        (outl, outr) = (inr, inl);
    tel;

    node id(inl, inr : int) returns (outl, outr : int);
    let
        (outl, outr) = swap(swap(inl, inr));
    tel;
}

fn main() {
    let (i, j) = (1, 2);
    let mut id = id::default();
    assert_eq!((i, j), id.step((i, j).embed()).trusted());
}
