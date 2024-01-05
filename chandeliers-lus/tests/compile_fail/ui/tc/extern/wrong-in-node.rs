//! Node exists but has a different signature.
use chandeliers_sem::*;

#[allow(non_camel_case_types)]
#[derive(Debug, Default)]
struct wrong_step_sig {}
impl wrong_step_sig {
    fn step(&mut self, _: ty!(i64)) -> ty!(f64) {
        unimplemented!()
    }
}
chandeliers_lus::decl! {
    extern node wrong_step_sig() returns (f: float);
}

fn main() {}
