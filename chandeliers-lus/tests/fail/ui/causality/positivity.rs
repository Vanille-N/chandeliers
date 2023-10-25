// Trying many combinations of depths.

chandeliers_lus::decl! {
    node normal(i : int) returns (o : int);
    let
        o = i;
    tel;
}

chandeliers_lus::decl! {
    node deep(i : int) returns (o : int);
    let
        o = 0 -> 0 -> pre pre i;
    tel;
}

chandeliers_lus::decl! {
    node deeper(i : int) returns (o : int);
    let
        o = 0 -> 0 -> 0 -> 0 -> 0 -> 0 -> 0 -> pre pre pre pre pre pre pre i;
    tel;
}

chandeliers_lus::decl! {
    node off_by_one(i : int) returns (o : int);
    let
        o = pre i;
    tel;
}

chandeliers_lus::decl! {
    node deep_off_by_one(i : int) returns (o : int);
    let
        o = 0 -> 0 -> pre pre pre i;
    tel;
}

chandeliers_lus::decl! {
    node deeper_off_by_one(i : int) returns (o : int);
    let
        o = 0 -> 0 -> 0 -> 0 -> 0 -> 0 -> 0 -> pre pre pre pre pre pre pre pre i;
    tel;
}

chandeliers_lus::decl! {
    node way_off(i : int) returns (o : int);
    let
        o = 0 -> 0 -> pre pre pre pre pre pre pre pre pre i;
    tel;
}

fn main() {}
