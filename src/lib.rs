use chandeliers_macros as lustre;

/*
lustre::decl! {
    node test(x, y, z : bool; w : int;) returns ()
    var s, t : float;
    let
        x = 0
    tel
}

lustre::decl! {
    node full_add(a, b, c : bool) returns (s, co : bool);
    let
      s = (a <> b) <> c;
      co = (a and b) or (a and c) or (b and c);
    tel;
}

lustre::decl! {
    node half_add(a, b : bool) returns (s, co : bool);
    let
      s = (a <> b);
      co = (a and b);
    tel;
}

lustre::decl! {
    node full_add_h(a, b, c : bool) returns (s, co : bool);
    var s1, c1, c2 : bool;
    let
      (s1, c1) = half_add(a, b);
      (s, c2) = half_add(c, s1);
      co = (c1 or c2);
    tel;
}

lustre::decl! {
    (* This is a comment *)
}

fn foo() {
    let mut x: i64;
    {lustre::asst_target! {
        x = 1
    }}

    let mut (x, y, z): (i64, i64, i64);
    {lustre::asst_target! {
        (x, y, z) = (1, 2, 3)
    }}

    let mut (s, (t, (u, v))): (i64, (i64, (i64, i64)));
    {lustre::asst_target! {
        (s, (t, (u, v))) = (1, (2, (3, 4)))
    }}

}
*/

fn main() {
    /*
    let _ = lustre::expr!(x);
    let _ = lustre::expr!(42);
    let _ = lustre::expr!((42, 0, 5));
    let _ = lustre::expr!(s * t * u * v * w * x * y * z);
    let _ = lustre::expr!(s - t + u - v + w - x + y + z);
    let _ = lustre::expr!((s - t + u - (v + w)) - (x + y + z));
    let _ = lustre::expr!(s * t / u - v + w * x * y + z);
    let _ = lustre::expr!(f(x));
    let _ = lustre::expr!(f(x, g(y), h(i(j()))));
    let _ = lustre::expr!(x -> y -> z);
    let _ = lustre::expr!(pre pre x);
    let _ = lustre::expr!(-x);
    let _ = lustre::expr!(not not x = y);
    let _ = lustre::expr!(x and y and z);
    let _ = lustre::expr!(x or y or z);
    */
}

//       -----------forbid--------->
//       ---alert-->     ---deny--->
// Nothing         Warning         Error
//       <--allow---    <-tolerate--
//       <----------ignore----------

/* Works
lustre::decl! {
    const A : int = B + Z;
    const B : int = Z;
    const C : int = A;
    extern const W : int;
    const D : int = C + E;
    const E : int = W + Z;
    extern const Z : int;
}
*/

lustre::decl! {
    const B : int = A + 1;
    const C : int = B + A;
    const A : int = 3;

    node foo(a : int; f : float) returns (b : bool);
    var m, n : int;
    let
        a = A;
        n = 1 + bar(1.0);
    tel;
    extern node bar(i : float) returns (n : int);
}

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
