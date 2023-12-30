use chandeliers_std::rand::random_bool;

chandeliers_lus::decl! {
    #[trace("random: b = {b}\n")]
    extern node random_bool() returns (b : bool);

    node random_incr() returns (i : int);
    let i = if random_bool() then 1 else 0; tel;

    #[trace("cumul:  n = {n}\n")]
    node cumulative_sum(n : int) returns (s : int);
    let s = (0 fby s) + n; tel;

    node increasing(n : int) returns (ok : bool);
    let ok = true -> (n >= pre n); tel;

    #[main(30)]
    #[trace("system: s = {s}\n\n")]
    node system() returns ();
    var s : int;
    let
        s = cumulative_sum(random_incr());
        assert increasing(s);
    tel;
}
