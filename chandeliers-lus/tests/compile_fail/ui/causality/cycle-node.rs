chandeliers_lus::decl! {
    node foo(i : int) returns (o : int);
    let o = bar(i); tel;

    node bar(i : int) returns (o : int);
    let o = foo(i); tel;
}

fn main() {}
