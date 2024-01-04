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

mod pipeline;

use chandeliers_err::{self as err, EAccum, Error};
use chandeliers_san::sp::Sp;
use chandeliers_syn as syntax;

// Dependencies for integration tests: silence the "unused dependencies" warning.
#[cfg(test)]
mod integration_deps {
    use chandeliers_sem as _;
    use chandeliers_std as _;
    use rand as _;
}

/// Frontend of `chandeliers_lus`.
///
/// Usage:
/// ```
/// chandeliers_lus::decl! {
///     #[trace[stderr]] // This means that every invocation of the node will print
///                      // debug information.
///     node count() returns (n : int);
///     let n = 0 fby n + 1; tel;
///
///     #[main(15)] // This means that this is the main function of the program.
///                 // It must have signature () -> () and will run 15 times.
///     node system() returns ();
///     var n : int;
///     let
///         n = count();
///         assert n > 0; // This will of course fail on the first iteration.
///                       // An assertion error will be printed and `system` will
///                       // abort.
///     tel
/// }
/// ```
#[proc_macro]
pub fn decl(i: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use syn::parse_macro_input;
    let prog = parse_macro_input!(i as Sp<syntax::Prog>);
    let mut eaccum = EAccum::default();

    let prog = prog_pipeline(&mut eaccum, prog);
    let fatal = eaccum.is_fatal();
    let (es, ws) = eaccum.fetch();
    let Some(prog) = prog else {
        err::consistency!(fatal, "No program generated, but no fatal error emitted");
        for e in es {
            emit(e, proc_macro::Level::Error);
        }
        return proc_macro2::TokenStream::new().into();
    };
    for w in ws {
        // FIXME: this could be a Warning
        emit(w, proc_macro::Level::Error);
    }
    prog
}

/// Apply all compiler passes.
fn prog_pipeline(eaccum: &mut EAccum, prog: Sp<syntax::Prog>) -> Option<proc_macro::TokenStream> {
    // Just let the trait impls from [pipeline] guide you...
    pipeline::CompilerPass::new(eaccum, prog)?
        .finish()
        .apply(eaccum)?
        .finish()
        .apply(eaccum)?
        .finish()
        .apply(eaccum)?
        .finish()
        .apply(eaccum)?
        .finish()
        .codegen()
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

// Our battery of tests
// Compile-Fail tests. These should fail to compile.
compiling!(fail_ui_causality with compile_fail in compile_fail/ui/causality/);
compiling!(fail_ui_syn with compile_fail in compile_fail/ui/syn/);
compiling!(fail_ui_tc with compile_fail in compile_fail/ui/tc/);
compiling!(fail_ui_options with compile_fail in compile_fail/ui/options/);
compiling!(fail_std with compile_fail in compile_fail/std/);
compiling!(fail_ui_clk with compile_fail in compile_fail/ui/clk/);
// Pass tests. These should compile and run.
compiling!(pass_ui with pass in pass/ui/);
compiling!(pass_syn with pass in pass/syn/);
compiling!(pass_std with pass in pass/std/);
compiling!(pass_fromslides with pass in pass/fromslides/);
compiling!(pass_given with pass in pass/given/);
compiling!(pass_options with pass in pass/options/);
compiling!(pass_poly with pass in pass/poly/);
compiling!(pass_registers with pass in pass/registers/);
// Warn tests, which are technically implemented as fail tests
// by having `#[deny(warnings)]`.
compiling!(warn_dead_code with compile_fail in warn/dead_code/);
compiling!(warn_options with compile_fail in warn/options/);
// Not shown here: there is one extra test in `tests/` which
// is a `fail` test.
// `assert-false` is expected to compile successfully, but fail at runtime.
// This is not supported by the test framework used here, so it's handled manually.

/// Emit one error message from a sequence of spans and associated hint messages.
fn emit(elements: Error, level: proc_macro::Level) -> proc_macro2::TokenStream {
    let mut elements = elements.into_iter();
    let Some((msg, span)) = elements.next() else {
        err::abort!("This error message is empty")
    };
    let Some(span) = span else {
        err::abort!("The very first error should always have an associated span")
    };
    let mut d = proc_macro::Diagnostic::spanned(
        span.unwrap(/* proc_macro2::Span */).unwrap(/* proc_macro::Span */),
        level,
        msg,
    );
    for (msg, span) in elements {
        if let Some(span) = span {
            d = d.span_note(
                span.unwrap(/* proc_macro2::Span */).unwrap(/* proc_macro::Span */),
                msg,
            );
        } else {
            d = d.note(msg);
        }
    }
    d.emit();
    proc_macro2::TokenStream::new()
}
