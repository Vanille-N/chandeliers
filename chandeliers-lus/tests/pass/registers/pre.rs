chandeliers_lus::decl! {
    #[universal_pre]
    #[trace("testing {x}\n")]
    node prev_testing(x: int) returns (y: int);
    let y = 0 -> pre x; tel;

    #[trace("witness {x}\n")]
    node prev_witness(x: int) returns (y: int);
    let y = 0 -> pre x; tel;

    #[main(10)]
    node main() returns ();
    var n: int;
    let
        n = 0 -> 5 -> 4 -> 3 -> (pre n + pre pre n * pre pre pre n) % 100;
        assert prev_testing(n) = prev_witness(n);
    tel;

}
