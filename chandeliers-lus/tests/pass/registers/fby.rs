//! Again using normal mode as a witness to test that `#[universal_pre]` works.
chandeliers_lus::decl! {
    #[universal_pre]
    #[trace("testing {x}\n")]
    node fby_testing(x: int) returns (y: int);
    let y = 0 fby x; tel;

    #[trace("witness {x}\n")]
    node fby_witness(x: int) returns (y: int);
    let y = 0 fby x; tel;

    #[main(10)]
    node main() returns ();
    var n: int;
    let
        n = 0 -> 5 -> 4 -> 3 -> (pre n + pre pre n * pre pre pre n) % 100;
        assert fby_testing(n) = fby_witness(n);
    tel;

}
