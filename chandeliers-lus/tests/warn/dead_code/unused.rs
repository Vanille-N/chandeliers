#![deny(dead_code)]

chandeliers_lus::decl! {
    node count1() returns (n : int);
    let n = 0 fby n + 1; tel;

    node count2() returns (n : int);
    let n = 0 fby n + 1; tel;

    node count3() returns (n : int);
    let n = 0 fby n + 1; tel;

    const X: int = 0;

    #[main]
    node system() returns ();
    let
        assert count1() = count3();
    tel
}
