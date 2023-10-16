use chandeliers_sem::*;

chandeliers_lus::decl! {
    node check (x: bool) returns (OK: bool);
    var n1, n2: int;
    let
      n1 = 0 -> pre n1 + 1;
      n2 = 1 -> pre n2 + 1;
      OK = (n1 + 1) = n2;
    tel
}

fn main() {
    let mut check = check::default();
    for b in [true, false, true, true, false, true, true, false, true] {
        assert!(check.update_mut(lit!(b)).unwrap());
    }
}


