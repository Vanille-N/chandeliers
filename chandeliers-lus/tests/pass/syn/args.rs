//! Testing the cast of `(())` to `()`.
chandeliers_lus::decl! {
    node zero() returns ();
    let tel;
    node test_zero() returns ();
    let () = zero((())); tel;

    node one(_i : int) returns ();
    let tel;
    node test_one() returns ();
    let () = one((1)); tel;

    node two(_i, _j : int) returns ();
    let tel;
    node test_two() returns ();
    let () = two((1, 2)); tel;

    node three(_i, _j, _k : int) returns ();
    let tel;
    node test_three() returns ();
    let () = three((1, 2, 3)); tel;
}

fn main() {}
