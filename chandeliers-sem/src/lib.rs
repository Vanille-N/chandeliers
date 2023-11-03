//! Tools for defining the semantics of Lustre.

#![feature(lint_reasons)]
#![warn(missing_docs)]
#![warn(
    unused_crate_dependencies,
    unused_macro_rules,
    variant_size_differences,
    clippy::allow_attributes,
    clippy::allow_attributes_without_reason,
    clippy::expect_used,
    clippy::indexing_slicing,
    clippy::missing_docs_in_private_items,
    clippy::missing_inline_in_public_items,
    clippy::multiple_inherent_impl,
    clippy::panic,
    clippy::pedantic,
    clippy::str_to_string,
    clippy::unreachable,
    clippy::unwrap_used,
    clippy::use_debug
)]

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
