#![feature(proc_macro_diagnostic)]

use quote::ToTokens;
use syn::parse_macro_input;

use chandeliers_san as sanitizer;
use chandeliers_syn as syntax;

use sanitizer::ast::Sp;
use sanitizer::causality::Causality;
use sanitizer::positivity::MakePositive;
use syntax::translate::SpanTranslate;

#[proc_macro]
pub fn decl(i: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let run_uid: usize = rand::random();
    let prog = parse_macro_input!(i as Sp<syntax::ast::Prog>);
    let prog = match prog.translate(run_uid, ()) {
        Ok(prog) => prog,
        Err(e) => return e.into(),
    };
    let mut prog = match prog.causality() {
        Ok(prog) => prog,
        Err(e) => return emit(e).into(),
    };
    match prog.typecheck() {
        Ok(()) => {}
        Err(e) => return emit(e).into(),
    }
    match prog.make_positive() {
        Ok(()) => {}
        Err(e) => return e.into(),
    }
    let mut toks = proc_macro2::TokenStream::new();
    prog.to_tokens(&mut toks);
    toks.into()
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

compiling!(pass_ui with pass in pass/ui/);
compiling!(pass_syn with pass in pass/syn/);
compiling!(pass_std with pass in pass/std/);
compiling!(pass_fromslides with pass in pass/fromslides/);
compiling!(pass_given with pass in pass/given/);

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
