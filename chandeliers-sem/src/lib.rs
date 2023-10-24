//! Tools for defining the semantics of Lustre.

// `candle` may generate comparisons `n >= 0` for `n: u64`
#![allow(unused_comparisons)]
#![feature(concat_idents)]

#[cfg(test)]
mod tests;

pub mod nillable;

pub mod time_travel;

pub mod stepping;

#[macro_use]
pub mod candle;

/// Stream traits.
pub mod traits {
    pub use crate::stepping::{DummyStep, Embed, Step, Trusted};
    pub use crate::time_travel::{Ago, Update};
}

/// The macros that define the semantics of Candle by translating it to Rust.
pub mod macros {
    /// Assertion:
    pub use crate::truth;
    /// Debugging:
    pub use crate::{assert_is, node_trace};
    /// Expression combinator:
    pub use crate::{binop, cmp, float, ifx, later, substep, unop};
    /// Expression:
    pub use crate::{lit, nil, var};
    /// Statement:
    pub use crate::{tick, update};
    /// Type:
    pub use crate::{ty, ty_mapping};
}
