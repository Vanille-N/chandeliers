//! Tests

#![feature(lint_reasons)]
#![warn(clippy::pedantic)]

use chandeliers_lus as lustre;

#[cfg(test)]
use chandeliers_sem::traits::Embed;
#[cfg(test)]
use chandeliers_sem::traits::Trusted;

use chandeliers_std::cast::float_of_int;

lustre::decl! {
    #[rustc_allow[dead_code]]
    node full_add(a, b, c : bool) returns (s, co : bool);
    let
      s = (a <> b) <> c;
      co = (a and b) or (a and c) or (b and c);
    tel;
}

lustre::decl! {
    #[export]
    #[doc("Half adder from two bits to one bit and one carry")]
    node half_add(a, b : bool) returns (s, co : bool);
    let
      s = (a <> b);
      co = (a and b);
    tel;
}

lustre::decl! {
    extern node half_add(a, b : bool) returns (s, co : bool);

    #[rustc_allow[dead_code]]
    node full_add_h(a, b, c : bool) returns (s, co : bool);
    var s1, c1, c2 : bool;
    let
      (s1, c1) = half_add(a, b);
      (s, c2) = half_add(c, s1);
      co = (c1 or c2);
    tel;
}

lustre::decl! {
    /* This is a comment */
    // This too
}

const W: i64 = 0;
const Z: i64 = 42;
lustre::decl! {
    #[rustc_allow[dead_code]]
    const A : int = B + Z;
    #[rustc_allow[dead_code]]
    const B : int = Z;
    #[rustc_allow[dead_code]]
    const C : int = A;
    extern const W : int;
    #[rustc_allow[dead_code]]
    const D : int = C + E;
    #[rustc_allow[dead_code]]
    const E : int = W + Z;
    extern const Z : int;
}

/*
lustre::decl! {
    const B2 : int = A2 + 1;
    const C2 : int = B2 + A2;
    const A2 : int = 3;

    node foo(a : int; f : float) returns (b : bool);
    var m, n : int;
    let
        n = 1 + bar(1.0) + A2;
        m = bar(0.2) + bar(1.3);
        b = n < m;
    tel;
    extern node bar(i : float) returns (n : int);
}
*/

/*
lustre::decl! {
    node equivalence(a, b, c : bool) returns (ok : bool);
    var s1, co1, s2, co2: bool;
    let
        a = 1;
        b = -true;
        c = 0.14;
        d = 1 -> 0 -> pre pre x;
        x = 0 fby (if b then 1 + 1 else 2);
        y = float(1);
        z = 0 fby foo(0 -> 1, (1, 2), x);
        y = float(1);
    tel
}
*/
/*
lustre::decl! {
    #[allow(unused_attr, recursive)]
    #[allow(unused_var, "a")]
    #[allow(non_positive_dependency)]
    #[allow(undeclared_type, "u64")]
    #[ignore(non_camel_case_name)]
    #[tolerate(kw_as_var, "const")]
    #[tracing("a", "b", "c", "ok")]
    node equivalence(a, b, c : bool) returns (ok : bool);
    var s1, co1, s2, co2: bool;
    let
      x = if b then 1 + 1 else 2;
      (s1, co1) = full_add(a, b, c);
      (s2, co2) = full_add_h(a, b, c);
      ok = (s1 = s2) and (co1 = co2);
      assert ok;
    tel
}
*/

/*
lustre::decl! {
    node baz(i : int) returns (m : int);
    let
        m = i;
    tel;

    const Z : int = X + 2;
    const X : int = 1 + (2 % 3) + 4 + 7;
    const Y : bool = (X = 5) or (X + 1 < 4);

    node foo(a, b, c : int; d, e, f : float) returns (g, h : bool);
    var m, n, o : int;
    let
        h = (n = 1);
        (n, o) = (0,1) fby (n,n+o);
        g = h;
        m = 0 -> pre baz(baz(baz(a + 1)));
    tel
}
*/

lustre::decl! {
    #[export]
    #[rustc_allow[dead_code]]
    node add(a, b : int) returns (sum : int);
    let
        sum = a + b;
    tel
}

#[test]
fn add_correct() {
    use chandeliers_sem::traits::*;
    let mut add = add::default();
    assert_eq!(add.step((5, 4).embed()).trusted(), 9);
}

lustre::decl! {
    extern node float_of_int(i : int) returns (f : float);

    #[pub]
    node test() returns ();
    var x : float;
    let
        x = float_of_int(5);
        assert x > 0.0;
    tel
}

#[test]
fn test_assertion() {
    let mut t = test::default();
    t.step(().embed());
}

lustre::decl! {
    extern node float_of_int(i : int) returns (f : float);

    #[pub]
    node failing() returns ();
    var x : float;
    let
        x = float_of_int(5);
        assert x <= 0.0;
    tel
}

#[test]
#[should_panic]
fn failing_assertion() {
    let mut t = failing::default();
    t.step(().embed());
}

lustre::decl! {
    node noio() returns ();
    let tel;

    #[rustc_allow[dead_code]]
    node system() returns (i : int);
    let
        i = 1;
        ((((((())), )),)) = (((noio(),)),);
    tel
}

lustre::decl! {
    node ret2() returns (i, j : int);
    let i = 1; j = 2; tel;

    #[rustc_allow[dead_code]]
    node system2() returns ();
    var _i, _j : int;
    let
        (_i, _j) = ret2();
    tel
}

/*
lustre::decl! {
    node dep() returns (y : int);
    let
        y = 1 -> y;
    tel
}
*/

lustre::decl! {
    #[rustc_allow[dead_code]]
    const X : int = 0;

    #[rustc_allow[dead_code]]
    node X() returns (X : int);
    let X = 1; tel
}

lustre::decl! {
    #[pub]
    node count() returns (x : int);
    let
        x = 0 -> pre x + 1;
    tel;
}

lustre::decl! {
    #[doc("Value of the Fibonacci sequence at index 0")]
    #[pub]
    const FIB0 : int = 0;
    #[doc("Value of the Fibonacci sequence at index 1")]
    #[pub]
    const FIB1 : int = 1;

    #[pub]
    node fib() returns (x : int);
    let
        x = FIB0 -> FIB1 -> pre x + pre pre x;
    tel;
}

lustre::decl! {
    extern node count() returns (out : int);

    #[pub]
    node counting_twice() returns (out : int);
    var b : bool;
    let
        b = true fby (not b);
        out = if true then count() else count();
    tel;

    #[pub]
    node counting_late() returns (out : int);
    let
        out = 0 fby 0 fby count();
    tel;
}

#[test]
fn fib_behavior() {
    let mut fib = fib::default();
    let mut vals = vec![];
    for _ in 0..10 {
        vals.push(fib.step(().embed()).trusted());
    }
    assert_eq!(&vals, &[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]);
}

#[test]
fn counting_twice_behavior() {
    let mut count = counting_twice::default();
    for i in 0..10 {
        let j = count.step(().embed()).trusted();
        assert_eq!(j, i);
    }
}

#[test]
fn counting_late_behavior() {
    let mut count = counting_late::default();
    for i in 0..10 {
        let j = count.step(().embed()).trusted();
        let actual_i = 0.max(i - 2);
        assert_eq!(j, actual_i);
    }
}

mod random {
    use chandeliers_sem::traits::Step;
    #[cfg(test)]
    use chandeliers_sem::traits::{Embed, Trusted};
    use chandeliers_sem::{implicit_clock, lit, ty};
    //use rand::Rng;

    #[derive(Default, Debug)]
    struct RandomInt {
        //rng: rand::rngs::ThreadRng,
    }

    impl Step for RandomInt {
        type Input = ();
        type Output = i64;
        fn step(&mut self, __inputs: ty!()) -> ty!(int) {
            implicit_clock!(__inputs);
            lit!(4) // chosen by fair dice roll
        }
    }

    chandeliers_lus::decl! {
        extern node RandomInt() returns (r : int);

        node sum(inc : int) returns (s : int);
        let
            s = inc + (0 fby s);
        tel;

        #[pub]
        #[rustc_allow[dead_code]]
        node randsum() returns (r, s : int);
        let
            r = RandomInt();
            s = sum(r);
        tel;
    }

    #[test]
    fn run() {
        let mut randsum = randsum::default();
        let mut sum = 0;
        for _ in 0..100 {
            let (r, s) = randsum.step(().embed()).trusted();
            sum += r;
            assert_eq!(sum, s);
        }
    }
}

/*
chandeliers_lus::decl! {
    extern node bar(i : int) returns (o : int);
    node foo(i : int) returns (o : int);
    let o = bar(i); tel;
}

chandeliers_lus::decl! {
    extern node foo(i : int) returns (o : int);
    node bar(i : int) returns (o : int);
    let o = foo(i); tel;
}
*/

chandeliers_lus::decl! {
    #[pub]
    node count1() returns (n : int);
    let n = 0 fby n + 1; tel
}

#[test]
fn test_count1() {
    use chandeliers_sem::traits::*;
    let mut count1 = count1::default();
    for _ in 0..10 {
        let _ = count1.step(().embed());
    }
}

chandeliers_lus::decl! {
    #[main]
    node foo() returns ();
    var n : int;
    let
        n = 0 fby n + 1;
        assert n < 10;
    tel;
}

mod testing {
    chandeliers_lus::decl! {
        #[export]
        node system() returns ();
        let assert true; tel;
    }

    mod sub {
        use super::system;
        chandeliers_lus::decl! {
            extern node system() returns ();

            #[pub]
            node main() returns ();
            let () = system(); tel;
        }
    }

    use sub::main;

    chandeliers_lus::decl! {
        #[main(10)]
        #[rustc_allow[dead_code]]
        extern node main() returns ();
    }

    chandeliers_lus::decl! {
        #[rustc_allow[dead_code]]
        node test() returns ();
        let tel;
    }
}

mod then_assoc {
    chandeliers_lus::decl! {
        node count() returns (n : int);
        let n = 0 fby n + 1; tel;

        node a() returns (i, j : int);
        let i = 0; j = count(); tel;
        node b() returns (i, j : int);
        let i = 1; j = count(); tel;
        node c() returns (i, j : int);
        let i = 2; j = count(); tel;

        #[trace]
        node testing() returns (i, j : int);
        let
            (i, j) = a() -> b() -> c();
        tel;

        node witness() returns (i, j : int);
        let
            (i, j) =
                (0, 0) ->
                (1, 1) ->
                (2, 2) ->
                (2, 3) ->
                (2, 4) ->
                (2, 5) ->
                (2, 6);
        tel;

        #[main(2)]
        #[rustc_allow[dead_code]]
        node system() returns ();
        var ti, tj, wi, wj : int;
        let
            (ti, tj) = testing();
            (wi, wj) = witness();
            assert ti = wi;
            assert tj = wj;
        tel;
    }
}

mod slow {
    chandeliers_lus::decl! {
        #[trace]
        node counter() returns (n : int);
        let n = 0 fby n + 1; tel;

        node blink() returns (b : bool);
        let b = true fby not b; tel;

        #[main]
        #[rustc_allow[dead_code]]
        node system() returns ();
        var b, m : bool; n : int when b; witness : int;
        let
            b = blink();
            witness = 0 fby (witness + if b then 1 else 0);
            n = counter(() when b);
            m = merge b (n = witness) true;
            assert m;
        tel;
    }
}

mod dead {
    chandeliers_lus::decl! {
        #[rustc_allow[dead_code]]
        node test() returns ();
        let tel
    }
}

mod by_trait {
    chandeliers_lus::decl! {
        #[trait]
        #[export]
        node test(i : int) returns (o : int);
        let o = i + (0 fby o); tel;
    }
}

mod fizzbuzz {
    chandeliers_lus::decl! {
       #[trace("Fizz")]
       node fizz() returns ();
       let tel;

       #[trace("Buzz")]
       node buzz() returns ();
       let tel;

       #[trace("{n}")]
       node id(n : int) returns ();
       let tel;

       #[trace("\n")]
       node newline() returns ();
       let tel;

       #[main(30)]
       node main() returns ();
       var n : int; div_3, div_5, div_either : bool;
       let
           n = 1 fby n + 1;
           div_3 = (n % 3) = 0;
           div_5 = (n % 5) = 0;
           div_either = div_3 or div_5;
           () = fizz(() when div_3);
           () = buzz(() when div_5);
           () = id(n whenot div_either);
           () = newline();
       tel;
    }
}

chandeliers_lus::decl! {
    #[trace("<- {_clk}", "-> {_clk}")]
    node addition_correct() returns ();
    let
        assert 1 + 1 = 2;
    tel
}
