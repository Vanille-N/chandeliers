use chandeliers_sem::traits::*;

chandeliers_lus::decl! {
    node verify(b : bool) returns ();
    let assert b; tel;

    node never_odd(i : int) returns ();
    let
        () = verify(i % 2 = 0);
    tel;
}

fn main() {
    let mut never_odd = never_odd::default();
    for i in [0i64, 2, 8, 4, 2] {
        never_odd.step(i.embed());
    }
}
