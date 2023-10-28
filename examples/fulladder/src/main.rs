//! In this example we show a nontrivial Lustre program with
//! the use of standard library functions and a `#[main]`.

// Make `random_bool` from the standard library known to Rust.
use chandeliers_std::rand::random_bool;

chandeliers_lus::decl! {
    // Standard Lustre definitions for nodes

    node full_add(a, b, c : bool) returns (s, co : bool);
    let
        s = (a <> b) <> c;
        co = (a and b) or (a and c) or (b and c);
    tel;

    node half_add(a, b : bool) returns (s, co : bool);
    let
        s = (a <> b);
        co = (a and b);
    tel;

    node full_add_h(a, b, c : bool) returns (s, co : bool);
    var s1, c1, c2 : bool;
    let
        (s1, c1) = half_add(a, b);
        (s, c2) = half_add(c, s1);
        co = (c1 or c2);
    tel;

    node equivalence(a, b, c : bool) returns (ok : bool);
    var s1, co1, s2, co2: bool;
    let
        (s1, co1) = full_add(a, b, c);
        (s2, co2) = full_add_h(a, b, c);
        ok = (s1 = s2) and (co1 = co2);
    tel;

    // Make `random_bool` from the standard library known to Lustre.
    extern node random_bool() returns (b : bool);

    // This is the `main` function:
    // - it must have empty inputs and outputs,
    // - it will run 100 times (use 0 if you want an infinite loop),
    // - there can be only one in the entire program
    //   (if you write another `fn main` then Rustc will complain)
    #[main(100)]
    node system() returns ();
    var a, b, c, ok : bool; enumerate : int;
    let
        enumerate = 0 fby enumerate + 1;

        a = random_bool();
        b = random_bool();
        c = random_bool();

        ok = equivalence(a, b, c);
        assert ok;
    tel;
}
