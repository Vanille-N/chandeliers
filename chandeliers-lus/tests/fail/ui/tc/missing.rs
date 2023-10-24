// In this file we check that the errors generated when a constant/node
// is missing or has a wrong signature are not too obscure.

use chandeliers_sem::*;

chandeliers_lus::decl! {
    extern const MISSING: float;
}

const WRONG_TY: i64 = 0;
chandeliers_lus::decl! {
    extern const WRONG_TY: float;
}

chandeliers_lus::decl! {
    extern node missing() returns ();
}

#[allow(non_camel_case_types)]
struct not_impl_step {}
chandeliers_lus::decl! {
    extern node not_impl_step() returns ();
}

#[allow(non_camel_case_types)]
struct wrong_step_sig {}
impl chandeliers_sem::traits::Step for wrong_step_sig {
    type Input = i64;
    type Output = f64;
    fn step(&mut self, _: ty!(i64)) -> ty!(f64) {
        unimplemented!()
    }
}
chandeliers_lus::decl! {
    extern node wrong_step_sig() returns (b: bool);
}

fn main() {}
