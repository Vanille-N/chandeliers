chandeliers_lus::decl! {
    node foo(b : bool; x, y : int) returns (i : int);
    let
        i = merge b (x when b) (y whenot b);
    tel;

    #[main(10)]
    node main() returns ();
    var b : bool;
        n, x, y, r1, r2 : int;
    let
        n = 0 fby 2 * n;
        b = true fby false fby false fby b;
        x = n % 7;
        y = (3 * n) % 17;
        // now the actual test
        r1 = foo(b, x, y);
        r2 = if b then x else y;
        assert r1 = r2;
    tel;
}
