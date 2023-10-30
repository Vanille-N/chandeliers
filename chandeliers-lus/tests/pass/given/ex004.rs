use chandeliers_sem::traits::*;

chandeliers_lus::decl! {
    node incr (tic: bool) returns (cpt: int);
    let
      cpt = (0 -> pre cpt) + if tic then 1 else 0;
    tel;

    node check (x: bool) returns (ok: bool);
    var cpt : int;
    let
      cpt = incr(x);
      ok = true -> (pre cpt <= cpt);
    tel
}

fn main() {
    let mut check = check::default();
    for b in [
        false, false, false, true, true, false, true, false, true, false, true,
    ] {
        assert!(check.step(b.embed()).trusted());
    }
}
