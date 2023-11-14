//! Perform verification of a Candle AST all the way down to code generation.
//!
//! The entry point for this entire crate is in constructing an AST
//! in `ast::Prog`, and then you should do the following in exactly that order
//! (some steps assume that the previous is completed without errors)
//!
//! 0. From a parsed result, construct an `ast::Prog`.
//!
//! 1. Resolving causality.
//!     by `causality::Causality`
//!
//!     (will perform rearrangements of the declarations so
//!     that there are no cycles)
//!
//! 2. Typechecking
//!     by `typecheck::TypeCheckStmt`
//!
//!     (if any causality errors remain they may or may not
//!     appear as "unknown variable" errors at this stage,
//!     but these errors are both less reliable and less
//!     understandable than the ones triggered in `causality`)
//!
//! 3. Depth resolution
//!     by `positivity::MakePositive`
//!
//! 4. Codegen
//!     by `quote::ToTokens` in `codegen`
//!
//!     (errors remaining at this stage will almost always
//!     become hard compilation errors by Rustc, which will
//!     invariably be an order of magnitude harder to debug)
//!
//! All of the above steps are optional from the point of view of the
//! type system because at the end of step 0 you already have an AST
//! that you could do codegen on, but skipping them will very likely
//! result in at the best compilation errors that are harder to understand
//! (e.g. failure to uphold causality might result in Rustc complaining
//! that some type has a size unknown at compile-time and that you should
//! insert a Box, this is a symptom and clearly not a cause of the issue)
//! and at the worst code that compiles but may produce a `Nil` ouput,
//! which will then be a runtime issue.
//!
//! ...or you could just use a proc macro from a parent crate that will
//! automatically perform all these steps in the correct order and collect
//! errors.
//!
//! Be careful that some steps take `&mut ast::Prog` and return
//! `Result<(), TokenStream>`, while some steps instead take `ast::Prog`
//! and return `Result<ast::Prog, TokenStream>`.
//! There are internal implementation details that make these things difficult
//! to implement in other ways, but you should be careful not to drop your
//! AST because you thought it was changed in-place.

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

pub mod ast;
pub mod candle;
pub mod causality;
//pub mod clockcheck;
pub mod codegen;
pub mod positivity;
pub mod sp;
pub mod typecheck;
