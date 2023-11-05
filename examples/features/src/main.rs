use chandeliers_std::rand::random_float;

chandeliers_lus::decl! {
    #[trace[stderr]]
    extern node random_float() returns (f : float);

    #[export]
    #[rustc_allow[dead_code]]
    #[doc("Probability that this entire test will fail on the first execution.")]
    const PROBA_FAIL: float = 0.9;

    #[rustc_allow[unused_variables]] // We could also rename `i` into `_i`
    #[doc("Return `true` with probability `PROBA_SUCCESS`.")]
    #[doc("Note: the input is ignored")]
    node ignore_input_and_pick_random(i : int) returns (b : bool);
    let
        b = random_float() <= PROBA_FAIL;
    tel;

    #[export]
    #[doc("Increasing counter starting at 0 or 1 with a certain probability.")]
    #[doc("Has a certain probability of returning the same value twice in a row.")]
    node unreliable_counter() returns (n : int);
    let
        n = (if ignore_input_and_pick_random(42) then 1 else 0)
            + (0 fby n);
    tel;

    #[export]
    #[trait]
    #[trace]
    #[doc("Strictly increasing counter starting at 0.")]
    node reliable_counter() returns (n : int);
    let
        n = 0 fby n + 1;
    tel;
}

chandeliers_lus::decl! {
    #[trace]
    extern node unreliable_counter() returns (n : int);
    extern node reliable_counter() returns (n : int);

    #[main(15)]
    #[pub]
    #[doc("Verify that the strictly increasing counter is at least as big")]
    #[doc("as the increasing counter. There is currently a bug on purpose")]
    #[doc("in the implementation because there is a certain probability that")]
    #[doc("the unreliable counter starts at 1.")]
    #[doc("This results in a `PROBA_SUCCESS` probability of this node failing")]
    #[doc("its execution after one step, and a `1 - PROBA_SUCCESS` probability")]
    #[doc("of it running forever.")]
    node system() returns ();
    var c1, c2 : int;
    let
        c1 = unreliable_counter();
        c2 = reliable_counter();
        assert c1 <= c2;
    tel;
}
