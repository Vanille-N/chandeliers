#![feature(proc_macro_diagnostic)]

use std::sync::atomic::{AtomicUsize, Ordering};

use chandeliers_err as err;
use chandeliers_san as sanitizer;
use chandeliers_syn as syntax;

use sanitizer::ast::Sp;

/// Generate unique identifiers for each macro invocation. We need this to avoid
/// name collisions in `extern node` and `extern const` declarations.
static RUN_UID: AtomicUsize = AtomicUsize::new(0);
fn new_run_uid() -> usize {
    RUN_UID.fetch_add(1, Ordering::SeqCst)
}

#[proc_macro]
pub fn decl(i: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use quote::ToTokens;
    use syn::parse_macro_input;
    let prog = parse_macro_input!(i as Sp<syntax::ast::Prog>);
    let prog = match prog_pipeline(prog) {
        Ok(prog) => prog,
        Err(e) => return emit(e).into(),
    };
    let mut toks = proc_macro2::TokenStream::new();
    prog.to_tokens(&mut toks);
    toks.into()
}

fn prog_pipeline(
    prog: Sp<syntax::ast::Prog>,
) -> Result<Sp<sanitizer::ast::decl::Prog>, err::Error> {
    use sanitizer::causality::Causality;
    use sanitizer::positivity::MakePositive;
    use syntax::translate::SpanTranslate;
    let run_uid = new_run_uid();
    let prog = prog.translate(run_uid, ())?;
    let mut prog = prog.causality()?;
    prog.typecheck()?;
    prog.make_positive()?;
    Ok(prog)
}

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

fn emit(elements: Vec<(String, Option<proc_macro2::Span>)>) -> proc_macro2::TokenStream {
    let mut elements = elements.into_iter();
    let (msg, span) = elements.next().unwrap();
    let mut d =
        proc_macro::Diagnostic::spanned(span.unwrap().unwrap(), proc_macro::Level::Error, msg);
    for (msg, span) in elements {
        if let Some(span) = span {
            d = d.span_note(span.unwrap(), msg)
        } else {
            d = d.note(msg)
        }
    }
    d.emit();
    proc_macro2::TokenStream::new()
}
