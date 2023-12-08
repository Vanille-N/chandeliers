chandeliers_lus::decl! {
    #[generics[T, U]]
    node swap(t0: T; u0: U) returns (u1: U; t1: T);
    let t1 = t0; u1 = u0; tel;

    #[main]
    node main() returns ();
    var x: int;
        y: float;
    let
        (x, y) = swap(swap(1, 0.5));
    tel;
}
