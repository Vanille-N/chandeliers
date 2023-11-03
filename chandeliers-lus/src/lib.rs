//! Declarative macros of the Chandeliers suite.
//!
//! This crate defines the `proc_macro`s provided to compile Lustre code.
//! For now only the `decl` macro is available to compile an entire program.

#![feature(lint_reasons)]
#![feature(proc_macro_diagnostic)]
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

use std::sync::atomic::{AtomicUsize, Ordering};

use chandeliers_err as err;
use chandeliers_san::{self as sanitizer, sp::Sp};
use chandeliers_syn as syntax;

/// Generate unique identifiers for each macro invocation. We need this to avoid
/// name collisions in `extern node` and `extern const` declarations.
fn new_run_uid() -> usize {
    /// Mutable state to generate unique identifiers.
    static RUN_UID: AtomicUsize = AtomicUsize::new(0);
    RUN_UID.fetch_add(1, Ordering::SeqCst)
}

/// Frontend of `chandeliers_lus`.
///
/// Usage:
/// ```
/// chandeliers_lus::decl! {
///     #[trace] // This means that every invocation of the node will print
///              // debug information.
///     node foo() returns (n : int);
///     let n = 0 fby n + 1; tel;
///
///     #[main(15)] // This means that this is the main function of the program.
///                 // It must have signature () -> () and will run 15 times.
///     node system() returns ();
///     var n : int;
///     let
///         n = count;
///         assert n > 0;
///     tel
/// }
/// ```
#[proc_macro]
pub fn decl(i: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use quote::ToTokens;
    use syn::parse_macro_input;
    let prog = parse_macro_input!(i as Sp<syntax::Prog>);
    let prog = match prog_pipeline(prog) {
        Ok(prog) => prog,
        Err(e) => return emit(e).into(),
    };
    let mut toks = proc_macro2::TokenStream::new();
    prog.to_tokens(&mut toks);
    toks.into()
}

/// Run the entire process on the program: translation + causality check + typechecking +
/// positivity + clockchecking.
fn prog_pipeline(prog: Sp<syntax::Prog>) -> Result<Sp<sanitizer::ast::decl::Prog>, err::Error> {
    use sanitizer::causality::Causality;
    use sanitizer::clockcheck::ClockCheck;
    use sanitizer::positivity::MakePositive;
    use syntax::translate::SpanTranslate;
    let run_uid = new_run_uid();
    let prog = prog.translate(run_uid.into(), ())?;
    let mut prog = prog.causality()?;
    prog.typecheck()?; // FIXME: trait for this
    prog.clockcheck()?;
    prog.make_positive()?;
    // FIXME: clockchecking coming soon.
    Ok(prog)
}

/// Generate a run of trybuild test cases.
/// Usage: `compiling!(test_name with expected_outcome in path/to/test/folder)`.
macro_rules! compiling {
    ($fun:ident with $testing:ident in $($dir:ident / )*) => {
        #[test]
        fn $fun() {
            let t = trybuild::TestCases::new();
            t.$testing(concat!("tests/", $( concat!(stringify!($dir), "/") , )* "**/*.rs"));
        }
    };
}

compiling!(fail_ui_causality with compile_fail in fail/ui/causality/);
compiling!(fail_ui_syn with compile_fail in fail/ui/syn/);
compiling!(fail_ui_tc with compile_fail in fail/ui/tc/);
compiling!(fail_ui_options with compile_fail in fail/ui/options/);
compiling!(fail_std with compile_fail in fail/std/);
compiling!(fail_ui_clk with compile_fail in fail/ui/clk/);

compiling!(pass_ui with pass in pass/ui/);
compiling!(pass_syn with pass in pass/syn/);
compiling!(pass_std with pass in pass/std/);
compiling!(pass_fromslides with pass in pass/fromslides/);
compiling!(pass_given with pass in pass/given/);
compiling!(pass_options with pass in pass/options/);

/// Emit one error message from a sequence of spans and associated hint messages.
fn emit(elements: Vec<(String, Option<proc_macro2::Span>)>) -> proc_macro2::TokenStream {
    let mut elements = elements.into_iter();
    let Some((msg, span)) = elements.next() else {
        err::panic!("This error message is empty")
    };
    let Some(span) = span else {
        err::panic!("The very first error should always have an associated span")
    };
    let mut d = proc_macro::Diagnostic::spanned(span.unwrap(), proc_macro::Level::Error, msg);
    for (msg, span) in elements {
        if let Some(span) = span {
            d = d.span_note(span.unwrap(), msg);
        } else {
            d = d.note(msg);
        }
    }
    d.emit();
    proc_macro2::TokenStream::new()
}
