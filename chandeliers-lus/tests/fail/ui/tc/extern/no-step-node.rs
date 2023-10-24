#[allow(non_camel_case_types)]
#[derive(Default)]
struct not_impl_step {}
chandeliers_lus::decl! {
    extern node not_impl_step() returns ();
}

fn main() {}
