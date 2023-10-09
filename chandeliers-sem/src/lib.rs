//! Tools for defining the semantics of Lustre.

// `std::intrinsics::likely` is used by `candle` for clock comparisons
#![feature(core_intrinsics)]
// `candle` may generate comparisons `n >= 0` for `n: u64`
#![allow(unused_comparisons)]

#[cfg(test)]
mod tests;

pub mod nillable;

pub mod time_travel;

#[macro_use]
pub mod candle;

/// Stream traits.
pub mod traits {
    pub use crate::time_travel::{Ago, Update};
}

/// The macros that define the semantics of Candle by translating it to Rust.
pub mod macros {
    /// Type:
    pub use crate::ty;
    /// Debugging:
    pub use crate::{assert_is, node_trace};
    /// Expression combinator:
    pub use crate::{binop, cmp, float, ifx, substep, later, unop};
    /// Expression:
    pub use crate::{lit, nil, var};
    /// Statement:
    pub use crate::{tick, update};
}
