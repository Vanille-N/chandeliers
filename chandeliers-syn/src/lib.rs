//! Common entry point for the proc macros of the Chandeliers suite,
//! provides parsing for all Lustre constructs and translation to the
//! analysable AST of `chandeliers-san`
//!
//! The structure of the AST is not stable because the grammar of
//! the parseable fragment of Lustre could very well be extended later.

pub mod ast;

pub mod translate;

mod test;
