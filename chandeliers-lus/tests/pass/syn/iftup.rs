//! If can accept arbitrary types.
use chandeliers_sem::traits::*;

chandeliers_lus::decl! {
    #[export]
    node select(i : bool; a1, b1, c1, a2, b2, c2 : int) returns (a, b, c : int);
    let
        (a, b, c) = if i then (a1, b1, c1) else (a2, b2, c2);
        () = if not i then () else ();
        ((), ()) = ((), ());
    tel
}

fn main() {
    let mut select = select::default();
    assert_eq!(
        (1, 2, 3),
        select.step((true, 1, 2, 3, 11, 12, 13).embed()).trusted()
    );
    assert_eq!(
        (11, 12, 13),
        select.step((false, 1, 2, 3, 11, 12, 13).embed()).trusted()
    );
}
