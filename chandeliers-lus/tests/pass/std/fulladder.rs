chandeliers_lus::decl! {
    node full_add(a, b, c : bool) returns (s, co : bool);
    let
        s = (a <> b) <> c;
        co = (a and b) or (a and c) or (b and c);
    tel;

    node half_add(a, b : bool) returns (s, co : bool);
    let
        s = (a <> b);
        co = (a and b);
    tel;

    node full_add_h(a, b, c : bool) returns (s, co : bool);
    var s1, c1, c2 : bool;
    let
        (s1, c1) = half_add(a, b);
        (s, c2) = half_add(c, s1);
        co = (c1 or c2);
    tel;

    node equivalence(a, b, c : bool) returns (ok : bool);
    var s1, co1, s2, co2: bool;
    let
        (s1, co1) = full_add(a, b, c);
        (s2, co2) = full_add_h(a, b, c);
        ok = (s1 = s2) and (co1 = co2);
    tel;
}

fn main() {
    use chandeliers_sem::*;
    let mut eq = equivalence::default();
    for a in [true, false] {
        for b in [true, false] {
            for c in [true, false] {
                assert!(eq.update_mut(lit!(a), lit!(b), lit!(c)).unwrap())
            }
        }
    }
}
