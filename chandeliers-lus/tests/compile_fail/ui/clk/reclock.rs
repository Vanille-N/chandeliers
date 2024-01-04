//! Attempts to use bad clocks, including
//! clocks at level 2 (when the clock expression is itself clocked).
chandeliers_lus::decl! {
    node foo() returns ();
    var b0 : bool;
        b1a, b1b, b1c : bool when b0;
        b2a, b2b, b2c : bool when b1a;
    let
        // level 0
        b0 = true fby not b0;
        // level 1
        b1a = true when b0;
        b1b = true when b1a; // Error
        b1c = b1a;
        // level 2
        b2a = true when b1a;
        b2b = (not b1c) when b1a;
        b2c = (not b0) when b1a; // Error
    tel;
}

fn main() {}
