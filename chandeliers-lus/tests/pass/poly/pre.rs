// Yet another generic `pre` operator.
// There are a lot of those.
chandeliers_lus::decl! {
    #[generic[T]]
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
