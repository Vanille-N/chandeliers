//! Testing `#[universal_pre]` by comparing its output to the
//! -- at this point trusted -- output of a node in the default temporal mode.
chandeliers_lus::decl! {
    node foo() returns (i : int);
    let i = 0 fby i + 1; tel;

    #[main(100)]
    #[universal_pre]
    #[trace("{toggle}=> {witness}=={testing}\n")]
    node main() returns ();
    var toggle : bool;
        witness, testing : int when toggle;
    let
        toggle = true fby false fby false fby toggle;
        witness = foo(() when toggle);
        testing = (0 when toggle) fby testing + 1;
        assert merge toggle (witness = testing) true;
    tel;
}
