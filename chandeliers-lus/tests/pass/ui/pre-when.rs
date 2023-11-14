chandeliers_lus::decl! {
    #[trace("when     {i}\n")]
    node print_when(i : int) returns (); let tel;
    #[trace("when pre    {i}\n")]
    node print_when_pre(i : int) returns (); let tel;
    #[trace("pre when       {i}\n")]
    node print_pre_when(i : int) returns (); let tel;

    #[trace("-----\n")]
    node endstep() returns (); let tel;

    node foo(x : int) returns ();
    var b : bool;
    let
        b = true fby not b;
        () = print_when(x when b);
        () = () -> print_when_pre((pre x) when b);
        () = () -> print_pre_when(pre (x when b));
    tel;

    #[main(10)]
    node main() returns ();
    var n : int;
    let
        n = 0 fby n + 1;
        () = foo(n);
        () = endstep();
    tel;
}
