use chandeliers_sem::traits::*;

chandeliers_lus::decl! {
    #[export]
    node check (_x: bool) returns (OK: bool);
    var n1, n2: int;
    let
      n1 = 0 -> pre n1 + 1;
      n2 = 0 -> (1 -> pre (n2 + 1));
      OK = n1 = n2;
    tel
}

fn main() {
    let mut check = check::default();
    for b in [true, true, false, true, true, false, true] {
        assert!(check.step(b.embed()).trusted());
    }
}
