//! Testing a way to have `pre` on arbitrary expressions by using
//! generics.
chandeliers_lus::decl! {
    #[generic[T]]
    node ipre(t, init: T) returns (pt: T);
    let pt = init -> pre t; tel;

    #[main(10)]
    #[trace("{p}:{q} ")]
    node main() returns ();
    var b : bool;
        n : int;
        p,q: int when b;
    let
        b = true fby true fby false fby b;
        n = 1 fby n + 1;
        p = n when b;
        q = ipre((n, 0) when b);
    tel;
}
