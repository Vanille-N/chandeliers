//! Some standard counters, checked against each other.
chandeliers_lus::decl! {
    node counting() returns (n : int);
    let n = 0 fby n + 1; tel;

    node counting_twice() returns (n : int);
    var b : bool;
    let
        b = true fby not b;
        n = if b then counting() else counting();
    tel;

    node counting_late_fby() returns (n : int);
    let n = 0 fby 0 fby counting(); tel;

    node counting_late_then() returns (n : int);
    let n = 0 -> 0 -> 0 -> pre pre pre counting(); tel;

    node counting_catchup() returns (n : int);
    let n = 0 -> 0 -> 0 -> counting(); tel;

    #[main(10)]
    node system() returns ();
    var n : int;
    let
        n = 0 fby n + 1;
        assert counting_twice() = n;
        assert counting_late_fby() = (0 fby 0 fby n);
        assert counting_late_then() = (0 fby 0 fby 0 fby n);
        assert counting_catchup() = (0 -> 0 -> 0 -> n);
    tel;
}
