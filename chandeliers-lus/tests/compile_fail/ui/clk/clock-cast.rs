//! n is not under the right clock.
//! Even though 1 coerces to an arbitrary clock, it can't be used
//! directly for assignment.
chandeliers_lus::decl! {
    node count(_clk: int) returns (n : int);
    let n = 0 fby n + 1; tel;

    #[main(10)]
    #[trace("{m} =? {n}\n")]
    node main() returns ();
    var b : bool;
        n, m : int when b;
    let
        b = true fby not b;
        n = 1;
        m = 1 when b;
        assert (merge b (count(n) = count(m)) true);
    tel;
}

fn main() {}
