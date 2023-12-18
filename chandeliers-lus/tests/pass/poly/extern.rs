chandeliers_lus::decl! {
    #[generic[T]]
    #[export]
    #[trace("{_t}\n")]
    node show(_t: T) returns ();
    let tel;
}

chandeliers_lus::decl! {
    #[generic[T]]
    extern node show(t: T) returns ();

    #[main(1)]
    node main() returns ();
    let
        () = show(42);
        () = show(true);
        () = show(3.14);
    tel;
}
