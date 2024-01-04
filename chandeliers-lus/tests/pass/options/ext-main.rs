//! Testing the `#[main]` annotation on an `extern node`.
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
    extern node main() returns ();
}
