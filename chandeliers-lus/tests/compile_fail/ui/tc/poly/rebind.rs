chandeliers_lus::decl! {
    #[generic[T]]
    node alternate(x, y: T) returns (t: T);
    var b: bool;
    let
        b = true fby not b;
        t = if b then x else y;
    tel;

    node f() returns (v: int);
    let v = alternate(1, true); tel;
}

fn main() {}
