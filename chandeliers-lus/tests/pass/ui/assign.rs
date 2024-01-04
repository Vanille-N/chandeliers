//! Assignments with tuple unpacking and trailing commas.
#![allow(unused_variables)]
// Increasingly weird but very valid assignment tuples.
chandeliers_lus::decl! {
    node test() returns ();
    var a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q : int;
    let
        a = 1;
        (b) = 2;
        ((c)) = 3;
        (d, (e), f) = (4, 5, 6);
        ((g, h), i) = ((7, ((8))), ((((9)))));
        ((((((j),)),))) = ((3,),);
        (k, (((l, m, n))), o, p, (q,)) = (5, (6, 7, 8), 9, ((10)), ((11,)),);

    tel
}

fn main() {}
