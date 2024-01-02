chandeliers_lus::decl! {
    #[universal_pre]
    node foo(i : int) returns (o : int);
    let o = pre i; tel;
}

fn main() {}
