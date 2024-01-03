chandeliers_lus::decl! {
    #[trace("witness {b}\n")]
    node witness() returns (b : bool);
    let b = true fby false fby false fby b; tel;

    #[universal_pre]
    #[trace("testing {b}\n")]
    node testing() returns (b : bool);
    let b = true fby false fby false fby b; tel;

    #[main(20)]
    node main() returns ();
    let
        assert testing() = witness();
    tel;
}
