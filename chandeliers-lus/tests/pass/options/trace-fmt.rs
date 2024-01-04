//! A fun test to showcase what `#[trace]` enables accidentally.
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
