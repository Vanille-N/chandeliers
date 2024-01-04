//! Attempts to do binary operations on expressions that don't have
//! the same speed.
chandeliers_lus::decl! {
    node ignore(x : int) returns ();
    let tel;

    node foo(b : bool) returns ();
    var d : bool;
        x : int when b;
        xd : int when d;
        y : int whenot b;
        yd : int whenot d;
        z : int;
    let
        d = b;
        x = 1 when b;
        xd = 1 when d;
        y = 2 whenot b;
        yd = 2 whenot d;
        z = 3;
        // KO
        () = ignore(x + y);
        () = ignore(x + z);
        () = ignore(y + x);
        () = ignore(y + z);
        () = ignore(z + x);
        () = ignore(z + y);
        () = ignore(x + xd);
        () = ignore(y + yd);
        // OK
        () = ignore(x + x);
        () = ignore(x + 1);
        () = ignore(y + y);
        () = ignore(y + 1);
        () = ignore(z + z);
        () = ignore(z + 1);
        () = ignore(1 + x);
        () = ignore(1 + y);
        () = ignore(1 + z);
    tel;
}

fn main() {}
