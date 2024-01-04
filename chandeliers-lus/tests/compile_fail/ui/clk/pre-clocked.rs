//! There is a non-implicit clock under the `pre`, this cannot work
//! unless `#[universal_pre]` is specified.
chandeliers_lus::decl! {
    node foo(b : bool) returns ();
    var v, w, x : int when b;
    let
        v = 4 when b;
        w = pre v;
        x = 0 -> v;
    tel;
}

fn main() {}
