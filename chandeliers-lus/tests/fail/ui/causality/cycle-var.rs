chandeliers_lus::decl! {
    node foo(i : int) returns (o : int);
    var v, w : int;
    let
        v = o;
        w = i + v;
        o = w;
    tel;
}

fn main() {}
