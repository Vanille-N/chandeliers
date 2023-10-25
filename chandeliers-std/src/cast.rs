//! "Standard library" of Candle.

#![allow(non_camel_case_types)]

use chandeliers_sem::traits::*;
use chandeliers_sem::*;

#[derive(Debug, Clone, Default)]
pub struct float_of_int {}
impl Step for float_of_int {
    type Input = i64;
    type Output = f64;
    #[inline(always)]
    fn step(&mut self, inputs: ty!(i64)) -> ty!(f64) {
        inputs.map(|i| i as f64)
    }
}

#[derive(Debug, Clone, Default)]
pub struct ceil {}
impl Step for ceil {
    type Input = f64;
    type Output = i64;
    #[inline(always)]
    fn step(&mut self, inputs: ty!(f64)) -> ty!(i64) {
        inputs.map(|f| f.ceil() as i64)
    }
}

#[derive(Debug, Clone, Default)]
pub struct floor {}
impl Step for floor {
    type Input = f64;
    type Output = i64;
    #[inline(always)]
    fn step(&mut self, inputs: ty!(f64)) -> ty!(i64) {
        inputs.map(|f| f.floor() as i64)
    }
}
