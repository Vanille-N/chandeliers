//! Random primitives for Lustre.

use rand::{rngs::ThreadRng, Rng};

use chandeliers_sem::traits::{Embed, Step};
use chandeliers_sem::{implicit_clock, ty};

/// Lustre node that returns a random `int` uniformly between
/// `i64::MIN` and `i64::MAX`.
#[derive(Debug, Default, Clone)]
pub struct random_int {
    /// Internal random number generator.
    rng: ThreadRng,
}
impl Step for random_int {
    type Input = ();
    type Output = i64;
    fn step(&mut self, __inputs: ty!()) -> ty!(int) {
        implicit_clock!(__inputs);
        self.rng.gen::<i64>().embed()
    }
}

/// Lustre node that returns a random `float` uniformly between
/// `f64::MIN` and `f64::MAX`.
#[derive(Debug, Default, Clone)]
pub struct random_float {
    /// Internal random number generator.
    rng: ThreadRng,
}
impl Step for random_float {
    type Input = ();
    type Output = f64;
    fn step(&mut self, __inputs: ty!()) -> ty!(float) {
        implicit_clock!(__inputs);
        self.rng.gen::<f64>().embed()
    }
}

/// Lustre node that returns a random `bool` uniformly.
#[derive(Debug, Default, Clone)]
pub struct random_bool {
    /// Internal random number generator.
    rng: ThreadRng,
}
impl Step for random_bool {
    type Input = ();
    type Output = bool;
    fn step(&mut self, __inputs: ty!()) -> ty!(bool) {
        implicit_clock!(__inputs);
        self.rng.gen::<bool>().embed()
    }
}
