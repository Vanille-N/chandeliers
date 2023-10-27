chandeliers_lus::decl! {
    node zero() returns ();
    let tel;
    node test_zero() returns ();
    let () = zero((())); tel;

    node one(i : int) returns ();
    let tel;
    node test_one() returns ();
    let () = one((1)); tel;

    node two(i, j : int) returns ();
    let tel;
    node test_two() returns ();
    let () = two((1, 2)); tel;

    node three(i, j, k : int) returns ();
    let tel;
    node test_three() returns ();
    let () = three((1, 2, 3)); tel;
}

fn main() {}
