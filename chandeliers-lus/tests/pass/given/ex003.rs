use chandeliers_sem::*;
chandeliers_lus::decl! {
    node check (x: bool) returns (OK: bool);
    var n1, n2: int;
        b1, b2: bool;
    let
      n1 = 0 -> pre n1 + 1;
      n2 = 0 -> pre (0 -> pre (n1 + 2));
      b1 = false -> true;
      b2 = false -> pre b1;
      OK = if b1 and b2 then n1 = n2 else true;
    tel
}

fn main() {
    let mut check = check::default();
    for b in [true, false, true, true, true, true, false, true, false, true] {
        assert!(check.update_mut(lit!(b)).unwrap());
    }
}
