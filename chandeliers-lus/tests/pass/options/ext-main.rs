chandeliers_lus::decl! {
    node system() returns ();
    let assert true; tel;
}

mod sub {
    use super::system;
    chandeliers_lus::decl! {
        extern node system() returns ();

        #[export]
        node main() returns ();
        let () = system(); tel;
    }
}

use sub::main;

chandeliers_lus::decl! {
    #[main(10)]
    extern node main() returns ();
}
