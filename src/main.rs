chandeliers_lus::decl! {
    node f() returns (i : int);
    let i = 0 fby i + 1; tel;

    #[main(10)]
    #[trace("{i}\n")]
    #[universal_pre]
    node zero_then_one() returns ();
    var
        i : int;
    let
        i = 99 -> pre f();
    tel;
}
