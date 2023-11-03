//! Type conversion functions.

use chandeliers_sem::traits::Step;
use chandeliers_sem::ty;

///
#[derive(Debug, Clone, Default)]
pub struct float_of_int {}

impl Step for float_of_int {
    type Input = i64;
    type Output = f64;
    #[inline]
    #[expect(clippy::cast_precision_loss, reason = "Required by the user")]
    fn step(&mut self, inputs: ty!(i64)) -> ty!(f64) {
        inputs.map(|i| i as f64)
    }
}

/// Lustre node that converts a `float` to an `int`.
#[derive(Debug, Clone, Default)]
pub struct int_of_float {}

impl Step for int_of_float {
    type Input = f64;
    type Output = i64;

    #[inline]
    #[expect(clippy::cast_possible_truncation, reason = "Required by the user")]
    fn step(&mut self, inputs: ty!(f64)) -> ty!(i64) {
        inputs.map(|i| i as i64)
    }
}

/// Rounds a `float` to the int above.
#[derive(Debug, Clone, Default)]
pub struct ceil {}

impl Step for ceil {
    type Input = f64;
    type Output = f64;

    #[inline]
    fn step(&mut self, inputs: ty!(f64)) -> ty!(f64) {
        inputs.map(f64::ceil)
    }
}

/// Rounds a `float` to the int below.
#[derive(Debug, Clone, Default)]
pub struct floor {}

impl Step for floor {
    type Input = f64;
    type Output = f64;

    #[inline]
    fn step(&mut self, inputs: ty!(f64)) -> ty!(f64) {
        inputs.map(f64::floor)
    }
}
