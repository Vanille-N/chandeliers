chandeliers_lus::decl! {
    #[main]
    node main() returns (i: int);
    let i = 0; tel;
}

chandeliers_lus::decl! {
    #[test]
    node test(i: int) returns (b: bool);
    let b = (i = 0); tel;
}

fn main() {}
