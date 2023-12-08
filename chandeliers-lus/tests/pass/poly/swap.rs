chandeliers_lus::decl! {
    #[generic[T, U]]
    #[trace("{t0} <-> {u0}\n")]
    node swap(t0: T; u0: U) returns (u1: U; t1: T);
    let t1 = t0; u1 = u0; tel;

    #[main(1)]
    node main() returns ();
    var _x: int;
        _y: float;
    let
        (_x, _y) = swap(swap(1, 0.5));
    tel;
}
