chandeliers_lus::decl! {
    node main(b : bool) returns ();
    var m, n : int when b;
    let (m, n) = (1, 2 when b); tel;
}

fn main() {}
