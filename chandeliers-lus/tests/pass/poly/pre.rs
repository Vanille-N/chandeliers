chandeliers_lus::decl! {
    #[generics[T]]
    node ipre(t, i: T) returns (o: T);
    let o = i -> pre t; tel;

    #[main]
    node main() returns ();
    var n : int;
    let
        n = 0 fby n + 1;
        assert ipre(n, -1) = (-1 fby n);
    tel;
}
