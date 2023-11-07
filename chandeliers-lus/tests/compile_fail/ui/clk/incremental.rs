chandeliers_lus::decl! {
    extern node foo(b : bool; x : int when b; y : int whenot b) returns (b2 : bool when b; b3 : bool when b2);
}
