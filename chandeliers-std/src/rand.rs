use rand::{rngs::ThreadRng, Rng};

use chandeliers_sem::traits::*;
use chandeliers_sem::*;

#[derive(Debug, Default, Clone)]
pub struct random_int {
    rng: ThreadRng,
}
impl Step for random_int {
    type Input = ();
    type Output = i64;
    fn step(&mut self, _: ()) -> ty!(int) {
        self.rng.gen::<i64>().embed()
    }
}

#[derive(Debug, Default, Clone)]
pub struct random_float {
    rng: ThreadRng,
}
impl Step for random_float {
    type Input = ();
    type Output = f64;
    fn step(&mut self, _: ()) -> ty!(float) {
        self.rng.gen::<f64>().embed()
    }
}

#[derive(Debug, Default, Clone)]
pub struct random_bool {
    rng: ThreadRng,
}
impl Step for random_bool {
    type Input = ();
    type Output = bool;
    fn step(&mut self, _: ()) -> ty!(bool) {
        self.rng.gen::<bool>().embed()
    }
}
