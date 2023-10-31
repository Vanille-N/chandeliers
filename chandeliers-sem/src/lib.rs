//! Tools for defining the semantics of Lustre.

// `candle` may generate comparisons `n >= 0` for `n: u64`
#![feature(concat_idents)]
#![allow(unused_comparisons)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]
#![warn(clippy::pedantic)]

#[cfg(test)]
mod tests;

pub mod nillable;

pub mod time_travel;

pub mod stepping;

#[macro_use]
pub mod candle;

/// Stream traits.
pub mod traits {
    pub use crate::nillable::AllNil;
    pub use crate::stepping::{Embed, Step, Trusted};
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
    /// Statement:
    pub use crate::{implicit_clock, tick, update};
    /// Expression:
    pub use crate::{lit, nil, var};
    /// Type:
    pub use crate::{ty, ty_mapping};
}
