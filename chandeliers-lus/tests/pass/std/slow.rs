//! Slowing down counters with clocks.
chandeliers_lus::decl! {
    #[trace[stderr]]
    node counter() returns (n : int);
    let n = 0 fby n + 1; tel;

    #[trace[stderr]]
    node blink() returns (b : bool);
    let b = true fby not b; tel;

    #[main(10)]
    node system() returns ();
    var b : bool; n1 : int when b; n2: int whenot b;
    let
        b = blink();
        n1 = counter(() when b);
        n2 = counter(() whenot b);
        assert (
            merge b n1 n2
            = 0 -> 0 -> 1 -> 1 -> 2 -> 2 -> 3 -> 3 -> 4 -> 4);
    tel;
}
