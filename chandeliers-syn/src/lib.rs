//! Common entry point for the proc macros of the Chandeliers suite,
//! provides parsing for all Lustre constructs and translation to the
//! analysable AST of `chandeliers-san`
//!
//! The structure of the AST is not stable because the grammar of
//! the parseable fragment of Lustre could very well be extended later.

#![feature(lint_reasons)]
#![warn(
    missing_docs,
    unused_crate_dependencies,
    unused_macro_rules,
    variant_size_differences,
    clippy::allow_attributes,
    clippy::allow_attributes_without_reason,
    clippy::expect_used,
    clippy::indexing_slicing,
    clippy::missing_docs_in_private_items,
    clippy::multiple_inherent_impl,
    clippy::panic,
    clippy::pedantic,
    clippy::str_to_string,
    clippy::unreachable,
    clippy::unwrap_used,
    clippy::use_debug
)]

pub(crate) mod ast;
pub use ast::Prog;

pub mod translate;

mod test;
